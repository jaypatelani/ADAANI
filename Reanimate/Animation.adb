with Graphics.SvgTree;
with Graphics.SvgTree.Printer;
with Reanimate.Constants;
with Reanimate.Ease;
with Reanimate.Svg.Constructors;
with Text.XML.Light.Output;

package Reanimate.Animation is

   -- | Duration of an animation or effect. Usually measured in seconds.
   type Duration is delta Double;
   -- | Time signal. Goes from 0 to 1, inclusive.
   type Time is Double;

   -- | SVG node.
   type SVG is Tree;

   -- | Animations are SVGs over a finite time.
   type Animation is record
      Duration : Duration;
      Time_Function : Time -> SVG;
   end record;

   -- * Creating animations
   function Mk_Animation (Duration : Duration; Time_Function : Time -> SVG) return Animation is
begin
   if Duration > 0.0 then
      return Unsafe_Mk_Animation (Duration, Time_Function);
   else
      raise Constraint_Error with "Animation duration (" & Duration'Img & ") is not positive.";
end Mk_Animation;

   function Unsafe_Mk_Animation (Duration : Duration; Time_Function : Time -> SVG) return Animation is
begin
   return Animation'(Duration, Time_Function);
end Unsafe_Mk_Animation;

   function Animate (Time_Function : Time -> SVG) return Animation is
begin
   return Animation'(1.0, Time_Function);
end Animate;

   function StaticFrame (Duration : Duration; SVG : SVG) return Animation;
   function Pause (Duration : Duration) return Animation;

   -- * Querying animations
   function Duration (Animation : Animation) return Duration;
   function Frame_At (Animation : Animation; Time : Time) return SVG;

   -- * Composing animations
   function Seq_A (Animation_1, Animation_2 : Animation) return Animation;
   function And_Then (Animation_1, Animation_2 : Animation) return Animation;
   function Par_A (Animation_1, Animation_2 : Animation) return Animation;
   function Par_Loop_A (Animation_1, Animation_2 : Animation) return Animation;
   function Par_Drop_A (Animation_1, Animation_2 : Animation) return Animation;

   -- * Modifying animations
   procedure Set_Duration (Animation : in out Animation; Duration : Duration);
   procedure Adjust_Duration (Animation : in out Animation; Delta : Duration);
   function Map_A (Animation : Animation; Map_Function : SVG -> SVG) return Animation;
   function Take_A (Animation : Animation; Count : Positive) return Animation;
   function Drop_A (Animation : Animation; Count : Positive) return Animation;
   function Last_A (Animation : Animation) return Animation;
   function Pause_At_End (Animation : Animation; Duration : Duration) return Animation;
   function Pause_At_Beginning (Animation : Animation; Duration : Duration) return Animation;
   function Pause_Around (Animation : Animation; Duration : Duration) return Animation;
   function Repeat_A (Animation : Animation; Count : Positive) return Animation;
   function Reverse_A (Animation : Animation) return Animation;
   function Play_Then_Reverse_A (Animation : Animation) return Animation;
   function Signal_A (Animation : Animation; Signal : Reanimate.Ease.Signal) return Animation;
   function Freeze_At_Percentage (Animation : Animation; Percentage : Positive) return Animation;
   function Add_Static (Animation : Animation; SVG : SVG) return Animation;

   -- * Misc
   function Get_Animation_Frame (Animation : Animation; Time : Time) return SVG;

   type Sync is ...;

   -- * Rendering
   function Render_Tree (Animation : Animation
