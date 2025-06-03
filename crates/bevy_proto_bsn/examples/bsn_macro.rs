//! BSN macro example
use bevy::{color::palettes::tailwind::*, prelude::*};
use bevy_proto_bsn::{Scene, *};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(BsnPlugin)
        .add_systems(Startup, |mut commands: Commands| {
            commands.spawn(Camera2d);
            commands.spawn_scene(ui());
        })
        .run();
}

// A little trick to get around the limitation of not being able to use consts with full/partial paths.
// Consts are only supported for single idents, or in braced expressions.
trait BorderRadiusConstExt {
    fn max() -> Self;
}

impl BorderRadiusConstExt for BorderRadius {
    #[inline]
    fn max() -> Self {
        Self::MAX
    }
}

#[derive(Clone, Component, Reflect)]
enum EnumStruct {
    MyStructVariant {
        field1: f32,
        field2: f32,
        field3: Vec3,
    },
}

impl Default for EnumStruct {
    fn default() -> Self {
        Self::MyStructVariant {
            field1: 0.0,
            field2: 0.0,
            field3: Vec3::ZERO,
        }
    }
}

#[derive(Clone, Component, Reflect)]
enum EnumTuple {
    MyTupleVariant(f32, Vec3),
}

impl Default for EnumTuple {
    fn default() -> Self {
        Self::MyTupleVariant(0.0, Vec3::ZERO)
    }
}

fn ui() -> impl Scene {
    pbsn! {
        Node {
            width: Val::Percent(100.0),
            height: Val::Percent(100.0),
            align_items: AlignItems::Center,
            justify_content: JustifyContent::Center,
            flex_direction: FlexDirection::Column,
            row_gap: px(5.0),
        } [
            Transform {
                translation: { x: 5.9 },
            },
            Visibility,
            (Node, Name::new("BasicButton"), :button("Basic")),
            (Node, :button("Rounded"), rounded),
            (Node { border: px_all(5.0) }, BorderColor(RED_500) :button("Thick red"), rounded),
            (Node, :button("Merged children"), rounded) [(
                Node {
                    width: px(30.0),
                    height: px(30.0),
                },
                BackgroundColor(BLUE_500),
                BorderRadius::max(),
            )],

            (:button("Click me!")) [
                // Observing parent entity
                On(|_: Trigger<Pointer<Click>>| {
                    info!("Clicked me!");
                })
            ],

            // Observing entity "BasicButton" by name
            On(|_: Trigger<Pointer<Click>>| {
                info!("Clicked Basic!");
            }, @"BasicButton"),
        ]
    }
}

fn button(text: &'static str) -> impl Scene {
    pbsn! {(
        Button,
        Node {
            padding: px_all(5.0),
            border: px_all(2.0),
            align_items: AlignItems::Center,
            column_gap: px(3.0),
        },
        BorderColor(LIME_800),
        BackgroundColor(LIME_500)
    ) [
        (Text::new(text), ConstructTextFont { font: @"Inter-Regular.ttf" })
    ]}
}

fn rounded() -> impl Scene {
    pbsn! {(
        BorderRadius::all(px(10.0))
    )}
}
