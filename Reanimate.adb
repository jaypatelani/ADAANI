with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Reanimate is

   -- Types
   type SVG is private;
   type Time is private;
   type Duration is private;
   type Animation is private;
   type Signal is private;
   type Scene is private;
   type ZIndex is private;
   type Var is private;
   type Sprite is private;
   type Frame is private;
   type Effect is private;
   type Sync is private;

   -- Functions
   function Reanimate (Filename : String) return Animation;
   function Reanimate_Live return Animation;
   function Reanimate_Live_Entry return Animation;
   function Mk_Animation (Frame : Time -> SVG) return Animation;
   function Animate (Animation : Animation; Time : Time) return SVG;
   function Static_Frame (SVG : SVG) return Animation;
   function Duration (Animation : Animation) return Duration;
   function Frame_At (Animation : Animation; Time : Time) return SVG;
   function Seq_A (Animation_1, Animation_2 : Animation) return Animation;
   function Par_A (Animation_1, Animation_2 : Animation) return Animation;
   function Par_Loop_A (Animation_1, Animation_2 : Animation) return Animation;
   function Par_Drop_A (Animation_1, Animation_2 : Animation) return Animation;
   function Pause (Duration : Duration) return Animation;
   function And_Then (Animation_1, Animation_2 : Animation) return Animation;
   function Map_A (Function : SVG -> SVG; Animation : Animation) return Animation;
   function Pause_At_End (Animation : Animation) return Animation;
   function Pause_At_Beginning (Animation : Animation) return Animation;
   function Pause_Around (Animation : Animation) return Animation;
   function Adjust_Duration (Animation : Animation; Duration : Duration) return Animation;
   function Set_Duration (Animation : Animation; Duration : Duration) return Animation;
   function Reverse_A (Animation : Animation) return Animation;
   function Play_Then_Reverse_A (Animation : Animation) return Animation;
   function Repeat_A (Animation : Animation) return Animation;
   function Freeze_At_Percentage (Animation : Animation; Percentage : Double) return Animation;
   function Add_Static (Animation : Animation; SVG : SVG) return Animation;
   function Signal_A (Animation : Animation; Duration : Duration; Signal : Signal) return Animation;
   function Constant_S (Value : Double) return Signal;
   function From_To_S (Start, End : Double) return Signal;
   function Reverse_S (Signal : Signal) return Signal;
   function Curve_S (Signal : Signal) return Signal;
   function Power_S (Signal : Signal; Exponent : Double) return Signal;
   function Bell_S (Signal : Signal) return Signal;
   function Oscillate_S (Signal : Signal) return Signal;
   function Cubic_Bezier_S (Signal : Signal; X1, Y1, X2, Y2 : Double) return Signal;
   function Scene (Animation : Animation) return Animation;
   procedure Play (
