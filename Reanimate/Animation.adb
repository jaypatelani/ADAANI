with Graphics.SvgTree;
with Reanimate.Ease;
with Reanimate.Svg.Constructors;

package Reanimate.Animation is

   -- | Duration of an animation or effect. Usually measured in seconds.
   type Duration is new Float;

   -- | Time signal. Goes from 0 to 1, inclusive.
   type Time is new Float;

   -- | SVG node.
   type SVG is new Graphics.SvgTree.Tree;

   -- | Animations are SVGs over a finite time.
   type Animation is record
      Duration : Duration;
      Frame : Time -> SVG;
   end record;

   -- | Construct an animation with a given duration. If the duration is not
   --   positive raises 'Constraint_Error'.
   function Mk_Animation (Duration : Duration; Frame : Time -> SVG) return Animation;

   -- | Construct an animation with a given duration, without checking that the
   --   duration is positive.
   function Unsafe_Mk_Animation (Duration : Duration; Frame : Time -> SVG) return Animation;

   -- | Construct an animation with a duration of @1@.
   function Animate (Frame : Time -> SVG) return Animation;

   -- | Create an animation with provided @duration@, which consists of stationary
   --   frame displayed for its entire duration.
   function Static_Frame (Duration : Duration; SVG : SVG) return Animation;

   -- | Query the duration of an animation.
   function Duration (Animation : Animation) return Duration;

   -- | Play animations in sequence. The @lhs@ animation is removed after it has
   --   completed. New animation duration is '@duration lhs + duration rhs@'.
   --
   --   Example:
   --
   --   @'Reanimate.Builtin.Documentation.Draw_Box' `'Seq_A'` 'Reanimate.Builtin.Documentation.Draw_Circle'@
   function Seq_A (LHS, RHS : Animation) return Animation;

   -- | Play two animation concurrently. Shortest animation freezes on last frame.
   --   New animation duration is '@max (duration lhs) (duration rhs)@'.
   --
   --   Example:
   --
   --   @'Reanimate.Builtin.Documentation.Draw_Box' `'Par_A'` 'Adjust_Duration' (*2) 'Reanimate.Builtin.Documentation.Draw_Circle'@
   function Par_A (LHS, RHS : Animation) return Animation;

   -- | Play two animation concurrently. Shortest animation repeats.
   --   New animation duration is '@max (duration lhs) (duration rhs)@'.
   --
   --   Example:
   --
   --   @'Reanimate.Builtin.Documentation.Draw_Box' `'Par_Loop_A'` 'Adjust_Duration' (*2) 'Reanimate.Builtin.Documentation.Draw_Circle'@
   function Par_Loop_A (LHS, RHS : Animation) return Animation;

   -- | Play two animation concurrently. LHS animation is removed after it has
   --   completed.
   --   New animation duration is '@duration rhs@'.
   --
   --   Example:
   --
   --   @'Reanimate.Built
