//! Super Sheep-Counter 2000
//!
//! An all-in-one numerical ruminant package.
//!
//! This example is originally from `i-cant-believe-its-not-bsn` and shows the differences between using `pbsn!` and `template!`.
use bevy::{
    color::palettes::css::{GREEN, RED},
    prelude::*,
};

use bevy_proto_bsn::{Scene, *};

fn main() {
    App::new()
        .register_hot_macro_source("examples")
        .add_plugins(DefaultPlugins)
        .add_plugins(BsnPlugin)
        .add_plugins(sheep_plugin)
        .run();
}

fn sheep_plugin(app: &mut App) {
    app.add_systems(Startup, setup)
        .add_systems(Update, sheep_system);
}

fn setup(mut commands: Commands) {
    commands.spawn(Camera2d);
    commands.spawn(UiRoot);
}

#[derive(Component)]
struct UiRoot;

#[derive(Component)]
struct Sheep;

// A query that pulls data from the ecs and then updates it using a template.
fn sheep_system(mut commands: Commands, sheep: Query<&Sheep>, root: Single<Entity, With<UiRoot>>) {
    let num_sheep = sheep.iter().len();

    let template = pbsn! {
        Node {
            position_type: PositionType::Absolute,
            bottom: Val::Px(15.0),
            right: Val::Px(16.0),
            row_gap: Val::Px(10.0),
            flex_direction: FlexDirection::Column,
        } [
            (Text("MY TEXT")),

            //(Text("MY TEXT ")),
            (Text("Testing"), TextFont { font_size: 80.0 }),

            Node {
                // padding: UiRect {
                //     left: Val::Px(10.0),
                //     right: Val::Px(15.0),
                //  //TODO: During reflection Default::default() can be omitted for nested types, but not in the EntityPatch...
                // }
            } [
                Text("MY TEXT 7s"),
            ],

            ( : counter(num_sheep, "sheep")),
        ]
    };

    commands.entity(*root).retain_scene(template);
}

// A function that returns an ecs template.
fn counter(num: usize, name: &'static str) -> impl Scene {
    pbsn! {
        //Node { padding: UiRect { left: Val::Px(50.0), right: Val::Px(50.0) } } [
        Node [
            Text("You have ") [
                TextSpan(format!("{num}")),
                TextSpan(format!(" {name}!")),
            ],
            (
                Button,
                Text("Increase"),
                TextColor(GREEN),
                visible_if(num < 100)
            ) [
                // Observes parent entity.
                // On(|_: Trigger<Pointer<Released>>, mut commands: Commands| {
                //     commands.spawn(Sheep);
                // })
            ],
            (
                Name::new("DecreaseButton"),
                Button,
                Text("Decrease"),
                TextColor(RED),
                visible_if(num > 0),
            ),
            // Observes named entity "DecreaseButton"
            // On(|_: Trigger<Pointer<Released>>, sheep: Query<Entity, With<Sheep>>, mut commands: Commands| {
            //     if let Some(sheep) = sheep.iter().next() {
            //         commands.entity(sheep).despawn();
            //     }
            // }, @"DecreaseButton"),
        ]
    }
}

// A component helper function for computing visibility.
fn visible_if(condition: bool) -> Visibility {
    if condition {
        Visibility::Visible
    } else {
        Visibility::Hidden
    }
}
