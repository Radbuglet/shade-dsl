use std::hash::{self, Hash};

use ctx2d_utils::hash::{FxHashMap, hash_map};
use glam::{Affine2, UVec2, Vec2};
use thunderdome::{Arena, Index};

use super::{
    CanvasUniformData, Crevice, InstanceRuns, RunIndex, TransformOffset,
    assets::{Asset, AssetKey, AssetLoader, AssetManager, AssetRetainer, ListKey, RefKey},
    buffer::{DynamicBufferHandle, DynamicBufferManager, DynamicBufferOpts},
    canvas_uniform_layout,
    depth::DepthGenerator,
    gfx_bundle::GfxContext,
    stream::StreamWrite,
    transform::TransformManager,
};

// === Brush Descriptors === //

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct RawBrushDescriptorRef<'a> {
    pub label: Option<&'a str>,
    pub vertex_module: &'a Asset<wgpu::ShaderModule>,
    pub vertex_entry: Option<&'a str>,
    pub fragment_module: &'a Asset<wgpu::ShaderModule>,
    pub fragment_entry: Option<&'a str>,
    pub instance_stride: wgpu::BufferAddress,
    pub instance_attributes: &'a [wgpu::VertexAttribute],
    pub uniforms: &'a [Option<&'a Asset<wgpu::BindGroupLayout>>],
}

#[derive(Debug)]
pub struct RawBrushDescriptor {
    pub label: Option<String>,
    pub vertex_module: Asset<wgpu::ShaderModule>,
    pub vertex_entry: Option<String>,
    pub fragment_module: Asset<wgpu::ShaderModule>,
    pub fragment_entry: Option<String>,
    pub instance_stride: wgpu::BufferAddress,
    pub instance_attributes: Vec<wgpu::VertexAttribute>,
    pub uniforms: Vec<Option<Asset<wgpu::BindGroupLayout>>>,
}

impl RawBrushDescriptorRef<'_> {
    pub fn intern(&self, assets: &mut impl AssetLoader) -> Asset<RawBrushDescriptor> {
        assets.load((), self, |_assets, (), me| me.to_owned_key())
    }
}

impl AssetKey for RawBrushDescriptorRef<'_> {
    type Owned = RawBrushDescriptor;

    fn delegated(&self) -> impl AssetKey<Owned = Self::Owned> + '_ {
        self
    }

    fn hash_key(&self, state: &mut impl hash::Hasher) {
        self.hash(state);
    }

    fn matches_key(&self, owned: &Self::Owned) -> bool {
        self.label == owned.label.as_deref()
            && self.vertex_module == &owned.vertex_module
            && self.vertex_entry == owned.vertex_entry.as_deref()
            && self.fragment_module == &owned.fragment_module
            && self.fragment_entry == owned.fragment_entry.as_deref()
            && self.instance_stride == owned.instance_stride
            && self.instance_attributes == owned.instance_attributes
            && self.uniforms.len() == owned.uniforms.len()
            && self
                .uniforms
                .iter()
                .zip(&owned.uniforms)
                .all(|(l, r)| *l == r.as_ref())
    }

    fn to_owned_key(&self) -> Self::Owned {
        RawBrushDescriptor {
            label: self.label.map(|v| v.to_string()),
            vertex_module: self.vertex_module.clone(),
            vertex_entry: self.vertex_entry.map(|v| v.to_string()),
            fragment_module: self.fragment_module.clone(),
            fragment_entry: self.fragment_entry.map(|v| v.to_string()),
            instance_stride: self.instance_stride,
            instance_attributes: self.instance_attributes.to_vec(),
            uniforms: self.uniforms.iter().map(|v| v.cloned()).collect(),
        }
    }
}

// === Brush Pipelines === //

fn load_pipeline_layout(
    assets: &mut impl AssetLoader,
    gfx: &GfxContext,
    layouts: &[&Asset<wgpu::BindGroupLayout>],
) -> Asset<wgpu::PipelineLayout> {
    assets.load(gfx, ListKey(layouts), |_assets, gfx, ListKey(layouts)| {
        gfx.device
            .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: None,
                bind_group_layouts: &layouts.iter().map(|v| &***v).collect::<Vec<_>>(),
                push_constant_ranges: &[],
            })
    })
}

#[derive(Debug, Clone)]
pub struct RawBrushPipelineDescriptor<'a> {
    pub descriptor: &'a Asset<RawBrushDescriptor>,
    pub color_format: wgpu::TextureFormat,
    pub depth_format: wgpu::TextureFormat,
    pub clip_mode: RawBrushClipMode,
    pub blend_state: Option<wgpu::BlendState>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum RawBrushClipMode {
    SetClip,
    ObeyClip,
    IgnoreClip,
}

impl AssetKey for RawBrushPipelineDescriptor<'_> {
    type Owned = (
        Asset<RawBrushDescriptor>,
        wgpu::TextureFormat,
        wgpu::TextureFormat,
        RawBrushClipMode,
        Option<wgpu::BlendState>,
    );

    fn delegated(&self) -> impl AssetKey<Owned = Self::Owned> + '_ {
        (
            RefKey(self.descriptor),
            RefKey(&self.color_format),
            RefKey(&self.depth_format),
            RefKey(&self.clip_mode),
            RefKey(&self.blend_state),
        )
    }
}

impl RawBrushPipelineDescriptor<'_> {
    pub fn load(
        &self,
        assets: &mut impl AssetLoader,
        gfx: &GfxContext,
    ) -> Asset<wgpu::RenderPipeline> {
        assets.load(gfx, self, |assets, gfx, me| {
            let layout = canvas_uniform_layout(assets, gfx);
            let layout = load_pipeline_layout(assets, gfx, &[&layout]);

            let stencil_state = match me.clip_mode {
                RawBrushClipMode::SetClip => Some(wgpu::StencilFaceState {
                    compare: wgpu::CompareFunction::Always,
                    fail_op: wgpu::StencilOperation::Replace,
                    depth_fail_op: wgpu::StencilOperation::Replace,
                    pass_op: wgpu::StencilOperation::Replace,
                }),
                RawBrushClipMode::ObeyClip => Some(wgpu::StencilFaceState {
                    compare: wgpu::CompareFunction::Equal,
                    fail_op: wgpu::StencilOperation::Keep,
                    depth_fail_op: wgpu::StencilOperation::Keep,
                    pass_op: wgpu::StencilOperation::Keep,
                }),
                RawBrushClipMode::IgnoreClip => None,
            };

            let stencil_state = stencil_state
                .map(|face_state| wgpu::StencilState {
                    front: face_state,
                    back: face_state,
                    read_mask: u32::MAX,
                    write_mask: u32::MAX,
                })
                .unwrap_or(wgpu::StencilState::default());

            gfx.device
                .create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                    label: me.descriptor.label.as_deref(),
                    layout: Some(&*layout),
                    vertex: wgpu::VertexState {
                        module: &me.descriptor.vertex_module,
                        entry_point: me.descriptor.vertex_entry.as_deref(),
                        compilation_options: wgpu::PipelineCompilationOptions::default(),
                        buffers: &[wgpu::VertexBufferLayout {
                            array_stride: me.descriptor.instance_stride,
                            step_mode: wgpu::VertexStepMode::Instance,
                            attributes: &me.descriptor.instance_attributes,
                        }],
                    },
                    primitive: wgpu::PrimitiveState {
                        topology: wgpu::PrimitiveTopology::TriangleList,
                        strip_index_format: None,
                        front_face: wgpu::FrontFace::Ccw,
                        cull_mode: None,
                        unclipped_depth: false,
                        polygon_mode: wgpu::PolygonMode::Fill,
                        conservative: false,
                    },
                    depth_stencil: Some(wgpu::DepthStencilState {
                        format: me.depth_format,
                        depth_write_enabled: true,
                        depth_compare: wgpu::CompareFunction::Greater,
                        stencil: stencil_state,
                        bias: wgpu::DepthBiasState::default(),
                    }),
                    multisample: wgpu::MultisampleState::default(),
                    fragment: Some(wgpu::FragmentState {
                        module: &me.descriptor.fragment_module,
                        entry_point: me.descriptor.fragment_entry.as_deref(),
                        compilation_options: wgpu::PipelineCompilationOptions::default(),
                        targets: &[Some(wgpu::ColorTargetState {
                            format: me.color_format,
                            blend: me.blend_state,
                            write_mask: wgpu::ColorWrites::all(),
                        })],
                    }),
                    multiview: None,
                    cache: None,
                })
        })
    }
}

// === RawCanvas Descriptors === //

#[derive(Debug)]
pub struct FinishDescriptor<'a> {
    pub encoder: &'a mut wgpu::CommandEncoder,
    pub color_attachment: &'a wgpu::TextureView,
    pub color_format: wgpu::TextureFormat,
    pub color_load: wgpu::LoadOp<wgpu::Color>,
    pub depth_attachment: &'a wgpu::TextureView,
    pub depth_format: wgpu::TextureFormat,
    pub width: u32,
    pub height: u32,
}

// === RawCanvas === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct RawBrushHandle(pub Index);

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
struct RawBrushXfHandle(pub Index);

#[derive(Debug)]
pub struct RawCanvas {
    assets: AssetManager,
    retainer: AssetRetainer,
    buffers: DynamicBufferManager,
    depth_gen: DepthGenerator,
    gfx: GfxContext,
    canvas_uniform_buffer: DynamicBufferHandle,
    transform: TransformManager,

    brushes: Arena<RawBrush>,
    brushes_xf: Arena<RawBrushXf>,
    last_xf_brush: Option<(RawBrushHandle, RawBrushXfHandle)>,

    commands: Vec<RawCommand>,
    blend: Option<wgpu::BlendState>,

    canvas_size: UVec2,
}

#[derive(Debug)]
enum RawCommand {
    Draw(RawBrushXfHandle, RunIndex),
    SetScissor([u32; 4]),
    UnsetScissor,
    SetBlend(Option<wgpu::BlendState>),
    StartClip,
    EndClip,
    UnsetClip,
}

#[derive(Debug)]
struct RawBrush {
    descriptor: Asset<RawBrushDescriptor>,
    uniforms: FxHashMap<u32, Asset<wgpu::BindGroup>>,
    transforms: FxHashMap<TransformOffset, RawBrushXfHandle>,
    last_xf: Option<(TransformOffset, RawBrushXfHandle)>,
}

#[derive(Debug)]
struct RawBrushXf {
    brush: RawBrushHandle,
    transform: TransformOffset,
    buffer: DynamicBufferHandle,
    runs: InstanceRuns,
}

impl RawCanvas {
    pub fn new(assets: AssetManager, gfx: GfxContext) -> Self {
        let retainer = AssetRetainer::new(assets.clone());
        let mut buffers = DynamicBufferManager::new(gfx.clone());
        let transform = TransformManager::new(&mut buffers);

        let canvas_uniform_buffer = buffers.create(DynamicBufferOpts {
            label: Some("canvas uniform buffer"),
            maintain_cpu_copy: false,
            usages: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::UNIFORM,
        });

        RawCanvas {
            assets,
            retainer,
            buffers,
            depth_gen: DepthGenerator::new(),
            gfx,
            canvas_uniform_buffer,
            transform,
            brushes: Arena::new(),
            brushes_xf: Arena::new(),
            last_xf_brush: None,
            commands: Vec::new(),
            blend: None,
            canvas_size: UVec2::ZERO,
        }
    }

    pub fn assets(&self) -> &AssetManager {
        &self.assets
    }

    pub fn gfx(&self) -> &GfxContext {
        &self.gfx
    }

    pub fn buffers(&self) -> &DynamicBufferManager {
        &self.buffers
    }

    pub fn buffers_mut(&mut self) -> &mut DynamicBufferManager {
        &mut self.buffers
    }

    pub fn set_canvas_size(&mut self, sz: UVec2) {
        self.canvas_size = sz;
    }

    #[must_use]
    pub fn canvas_size(&self) -> UVec2 {
        self.canvas_size
    }

    #[must_use]
    pub fn width(&self) -> u32 {
        self.canvas_size.x
    }

    #[must_use]
    pub fn height(&self) -> u32 {
        self.canvas_size.y
    }

    pub fn create_brush(&mut self, descriptor: Asset<RawBrushDescriptor>) -> RawBrushHandle {
        RawBrushHandle(self.brushes.insert(RawBrush {
            descriptor,
            uniforms: FxHashMap::default(),
            transforms: FxHashMap::default(),
            last_xf: None,
        }))
    }

    pub fn destroy_brush(&mut self, brush: RawBrushHandle) {
        todo!()
    }

    pub fn set_uniform(&mut self, brush: RawBrushHandle, idx: u32, group: Asset<wgpu::BindGroup>) {
        self.brushes[brush.0].uniforms.insert(idx, group);
    }

    #[must_use]
    pub fn transform(&self) -> Affine2 {
        self.transform.transform()
    }

    pub fn set_transform(&mut self, xf: Affine2) {
        self.transform.set_transform(xf);
        self.last_xf_brush = None;
    }

    pub fn apply_transform(&mut self, xf: Affine2) {
        self.set_transform(self.transform() * xf);
    }

    pub fn translate(&mut self, by: Vec2) {
        self.apply_transform(Affine2::from_translation(by));
    }

    pub fn scale(&mut self, by: Vec2) {
        self.apply_transform(Affine2::from_scale(by));
    }

    pub fn rotate_rad(&mut self, rad: f32) {
        self.apply_transform(Affine2::from_angle(rad));
    }

    pub fn rotate_deg(&mut self, deg: f32) {
        self.rotate_rad(deg.to_radians());
    }

    pub fn init_transform_gl(&mut self) {
        self.set_transform(Affine2::IDENTITY);
    }

    pub fn init_transform_painter(&mut self) {
        self.init_transform_gl();
        self.scale(Vec2::new(1., -1.));
        self.translate(-Vec2::ONE);
        self.scale(2.0 / self.canvas_size().as_vec2());
    }

    #[must_use]
    pub fn blend(&self) -> Option<wgpu::BlendState> {
        self.blend
    }

    pub fn set_blend(&mut self, state: Option<wgpu::BlendState>) {
        self.commands.push(RawCommand::SetBlend(state));
        self.blend = state;
    }

    pub fn set_scissor(&mut self, rect: Option<[u32; 4]>) {
        if let Some(rect) = rect {
            self.commands.push(RawCommand::SetScissor(rect));
        } else {
            self.commands.push(RawCommand::UnsetScissor);
        }
    }

    pub fn start_clip(&mut self) {
        self.commands.push(RawCommand::StartClip);
    }

    pub fn end_clip(&mut self) {
        self.commands.push(RawCommand::EndClip);
    }

    pub fn unset_clip(&mut self) {
        self.commands.push(RawCommand::UnsetClip);
    }

    #[must_use]
    pub fn depth(&self) -> f32 {
        self.depth_gen.depth()
    }

    pub fn draw(&mut self, brush: RawBrushHandle, instance_count: u32, data: &impl StreamWrite) {
        // Figure out the transformed brush to which we're drawing.
        let brush_xf = 'find_xf: {
            if let Some((last_brush, brush_xf)) = self.last_xf_brush {
                if last_brush == brush {
                    break 'find_xf brush_xf;
                }
            }

            if self.blend.is_some() {
                // We swapped brushes with blending enables, which requires us to place the
                // primitives in a new epoch.
                self.depth_gen.advance_epoch();
            }

            let state = &mut self.brushes[brush.0];

            let curr_xf = self.transform.transform_offset(&mut self.buffers);

            if let Some((last_xf, brush_xf)) = state.last_xf {
                if last_xf == curr_xf {
                    break 'find_xf brush_xf;
                }
            }

            let xf_entry = match state.transforms.entry(curr_xf) {
                hash_map::Entry::Occupied(entry) => {
                    let brush_xf = *entry.get();
                    state.last_xf = Some((curr_xf, brush_xf));
                    break 'find_xf brush_xf;
                }
                hash_map::Entry::Vacant(entry) => entry,
            };

            let buffer = self.buffers.create(DynamicBufferOpts {
                label: state.descriptor.label.as_deref(),
                maintain_cpu_copy: false,
                usages: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::VERTEX,
            });

            let brush_xf = RawBrushXfHandle(self.brushes_xf.insert(RawBrushXf {
                brush,
                transform: curr_xf,
                buffer,
                runs: InstanceRuns::new(),
            }));

            xf_entry.insert(brush_xf);
            state.last_xf = Some((curr_xf, brush_xf));

            brush_xf
        };

        self.last_xf_brush = Some((brush, brush_xf));

        // Push the instance
        let state = &mut self.brushes[brush.0];
        let state_xf = &mut self.brushes_xf[brush_xf.0];

        let written = self.buffers.extend(state_xf.buffer, data);

        debug_assert_eq!(
            written,
            instance_count as u64 * state.descriptor.instance_stride
        );

        if let Some(run_idx) = state_xf.runs.push(self.depth_gen.epoch(), instance_count) {
            self.commands.push(RawCommand::Draw(brush_xf, run_idx));
        }

        self.depth_gen.advance_depth();
    }

    pub fn finish_raw(&mut self, descriptor: FinishDescriptor<'_>) {
        let FinishDescriptor {
            encoder,
            color_attachment,
            color_format,
            color_load,
            depth_attachment,
            depth_format,
            width,
            height,
        } = descriptor;

        // Upload canvas state data.
        self.buffers.clear(self.canvas_uniform_buffer);
        self.buffers.extend(
            self.canvas_uniform_buffer,
            &Crevice(&CanvasUniformData {
                size_i32: UVec2::new(width, height).as_ivec2(),
                size_f32: UVec2::new(width, height).as_vec2(),
            }),
        );

        // Flush remaining buffers
        self.buffers.flush(encoder);

        // Render pass
        let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("canvas render pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: color_attachment,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: color_load,
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                view: depth_attachment,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(0.),
                    store: wgpu::StoreOp::Discard,
                }),
                stencil_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(0),
                    store: wgpu::StoreOp::Discard,
                }),
            }),
            timestamp_writes: None,
            occlusion_query_set: None,
        });

        let canvas_uniform_layout = canvas_uniform_layout(&mut self.retainer, &self.gfx);

        let canvas_uniform_group = self
            .gfx
            .device
            .create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("canvas uniform group"),
                layout: &canvas_uniform_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::Buffer(
                            self.buffers
                                .buffer(self.canvas_uniform_buffer)
                                .as_entire_buffer_binding(),
                        ),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Buffer(
                            self.buffers
                                .buffer(self.transform.buffer())
                                .as_entire_buffer_binding(),
                        ),
                    },
                ],
            });

        let mut clip_mode = RawBrushClipMode::IgnoreClip;
        let mut blend_state = None;
        let mut clip_idx = 0;

        for cmd in self.commands.drain(..) {
            match cmd {
                RawCommand::Draw(brush_xf, run_idx) => {
                    let state_xf = &self.brushes_xf[brush_xf.0];
                    let state = &self.brushes[state_xf.brush.0];

                    let pipeline = RawBrushPipelineDescriptor {
                        descriptor: &state.descriptor,
                        color_format,
                        depth_format,
                        clip_mode,
                        blend_state,
                    }
                    .load(&mut self.retainer, &self.gfx);

                    let instance_range = state_xf.runs.range(run_idx);

                    pass.set_pipeline(&pipeline);

                    pass.set_bind_group(0, &canvas_uniform_group, &[state_xf.transform.0]);

                    for (idx, group) in &state.uniforms {
                        pass.set_bind_group(*idx, &**group, &[]);
                    }

                    pass.set_vertex_buffer(0, self.buffers.buffer(state_xf.buffer).slice(..));
                    pass.draw(0..6, instance_range);
                }
                RawCommand::SetScissor([x, y, width, height]) => {
                    pass.set_scissor_rect(x, y, width, height);
                }
                RawCommand::UnsetScissor => {
                    pass.set_scissor_rect(0, 0, width, height);
                }
                RawCommand::SetBlend(new_blend_state) => {
                    blend_state = new_blend_state;
                }
                RawCommand::StartClip => {
                    clip_mode = RawBrushClipMode::SetClip;
                    pass.set_stencil_reference(clip_idx);
                    clip_idx += 1;
                }
                RawCommand::EndClip => {
                    clip_mode = RawBrushClipMode::ObeyClip;
                }
                RawCommand::UnsetClip => {
                    clip_mode = RawBrushClipMode::IgnoreClip;
                }
            }
        }

        drop(pass);

        // Reset encoder state
        self.retainer.reap();
        self.depth_gen.reset();
        self.transform.reset(&mut self.buffers);
        self.last_xf_brush = None;
        self.blend = None;

        for (_brush_xf, state_xf) in &mut self.brushes_xf {
            self.buffers.destroy(state_xf.buffer);
        }

        for (_brush, state) in &mut self.brushes {
            state.last_xf = None;
            state.transforms.clear();
        }
    }

    pub fn reclaim_raw(&mut self) {
        self.buffers.reclaim();
    }
}
