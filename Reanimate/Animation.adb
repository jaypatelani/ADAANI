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

   function Static_Frame (Duration : Duration; SVG : SVG) return Animation is
begin
   return Mk_Animation (Duration, (Time => SVG'Access));
end Static_Frame;

  function Duration (Animation : Animation) return Duration is
begin
   return Animation.Duration;
end Duration;



   -- * Composing animations
   function Seq_A (Animation_1, Animation_2 : Animation) return Animation is
begin
   Total_D := Animation_1.Duration + Animation_2.Duration;
   return Animation'(Total_D, (T =>
      if T < Animation_1.Duration / Total_D then
         Animation_1.Time_Function (T * Total_D / Animation_1.Duration)
      else
         Animation_2.Time_Function ((T - Animation_1.Duration / Total_D) * Total_D / Animation_2.Duration)));
end Seq_A;

  function Par_A (Animation_1, Animation_2 : Animation) return Animation is
begin
   Max_D := Max (Animation_1.Duration, Animation_2.Duration);
   return Animation'(Max_D, (T =>
      Mk_Group (T * Total_D / Max_D,
         Animation_1.Time_Function (Min (1.0, T * Total_D / Animation_1.Duration)),
         Animation_2.Time_Function (Min (1.0, T * Total_D / Animation_2.Duration)))));
end Par_A;

function Par_Loop_A (Animation_1, Animation_2 : Animation) return Animation is
begin
   Max_D := Max (Animation_1.Duration, Animation_2.Duration);
   return Animation'(Max_D, (T =>
      Mk_Group (T * Total_D / Max_D,
         Animation_1.Time_Function (Mod (T * Total_D / Animation_1.Duration, 1.0)),
         Animation_2.Time_Function (Mod (T * Total_D / Animation_2.Duration, 1.0)))));
end Par_Loop_A;

function Par_Drop_A (Animation_1, Animation_2 : Animation) return Animation is
begin
   Max_D := Max (Animation_1.Duration, Animation_2.Duration);
   return Animation'(Max_D, (T =>
      Mk_Group (T * Total_D / Max_D,
         if T * Total_D / Animation_1.Duration > 1.0 then
            None
         else
            Animation_1.Time_Function (T * Total_D / Animation_1.Duration),
         if T * Total_D / Animation_2.Duration > 1.0 then
            None
         else
            Animation_2.Time_Function (T * Total_D / Animation_2.Duration))));
end Par_Drop_A;

function Pause (Duration : Duration) return Animation is
begin
   return Mk_Animation (Duration, (T => None));
end Pause;

function And_Then (Animation_1, Animation_2 : Animation) return Animation is
begin
   return Par_A (Animation_1, Seq_A (Pause (Duration (Animation_1)), Animation_2));
end And_Then;

function Frame_At (Animation : Animation; Time : Time) return SVG is
begin
   T_P := Clamp (0.0, 1.0, Time / Animation.Duration);
   return Animation.Time_Function (T_P);
end Frame_At;

function Render_Tree (SVG : SVG) return String is
begin
   return XML_Of_Tree (SVG);
end Render_Tree;


function Render_SVG (Width : access Number; Height : access Number; SVG : SVG) return String is
   Width_ : Number := 16;
   Height_ : Number := 9;
begin
   return PP_Document (Document'(
      _Document_View_Box => Just ((-Width_/2, -Height_/2, Width_, Height_)),
      _Document_Width => Width,
      _Document_Height => Height,
      _Document_Elements => (With_Stroke_Width (Default_Stroke_Width, Scale_XY (1, -1, SVG))),
      _Document_Description => "",
      _Document_Location => "",
      _Document_Aspect_Ratio => Preserve_Aspect_Ratio'(False, Align_None, Nothing)));
end Render_SVG;


function Map_A (Animation : Animation; Map_Function : SVG -> SVG) return Animation is
begin
   return Animation'(Animation.Duration, (T => Map_Function (Animation.Time_Function (T))));
end Map_A;


function Pause_At_End (Animation : Animation; Duration : Duration) return Animation is
begin
   if Duration = 0.0 then
      return Animation;
   else
      return And_Then (Animation, Pause (Duration));
   end if;
end Pause_At_End;


function Pause_At_Beginning (Animation : Animation; Duration : Duration) return Animation is
begin
   if Duration = 0.0 then
      return Animation;
   else
      return Seq_A (Mk_Animation (Duration, (T => Freeze_Frame (0.0, Animation))), Animation);
   end if;
end Pause_At_Beginning;


function Pause_Around (Start, End : Duration; Animation : Animation) return Animation is
begin
   return Pause_At_End (Animation, End) & Pause_At_Beginning (Animation, Start);
end Pause_Around;


function Freeze_Frame (Time : Time; Animation : Animation) return Time -> SVG is
begin
   return (T => Animation.Time_Function (Time / Animation.Duration));
end Freeze_Frame;


function Adjust_Duration (Animation : Animation; Map_Function : Duration -> Duration) return Animation is
begin
   return Mk_Animation (Map_Function (Animation.Duration), Animation.Time_Function);
end Adjust_Duration;


function Set_Duration (Animation : Animation; New_Duration : Duration) return Animation is
begin
   return Adjust_Duration (Animation, (T => New_Duration));
end Set_Duration;


function Reverse_A (Animation : Animation) return Animation is
begin
   return Signal_A (Reverse_S, Animation);
end Reverse_A;


function Play_Then_Reverse_A (Animation : Animation) return Animation is
begin
   return Seq_A (Animation, Reverse_A (Animation));
end Play_Then_Reverse_A;


function Repeat_A (Animation : Animation; N : Double) return Animation is
begin
   return Mk_Animation (N * Animation.Duration, (T => Animation.Time_Function (Mod (T * N, 1.0))));
end Repeat_A;


function Freeze_At_Percentage (Animation : Animation; Fraction : Time) return Animation is
begin
   return Animation'(Animation.Duration, (T => Animation.Time_Function (Fraction)));
end Freeze_At_Percentage;


function Add_Static (Animation : Animation; Static : SVG) return Animation is
begin
   return Map_A (Animation, (Frame => Mk_Group (Static, Frame)));
end Add_Static;


function Signal_A (Animation : Animation; Signal : Signal) return Animation is
begin
   return Animation'(Animation.Duration, (T => Animation.Time_Function (Signal (T))));
end Signal_A;


function Take_A (Animation : Animation; Duration : Duration) return Animation is
begin
   if Duration >= Animation.Duration then
      return Animation;
   else
      return Mk_Animation (Duration, (T => Animation.Time_Function (T * Duration / Animation.Duration)));
   end if;
end Take_A;


function Drop_A (Animation : Animation; Duration : Duration) return Animation is
begin
   if Duration <= 0.0 then
      return Animation;
   else
      Rest := Animation.Duration - Duration;
      return Mk_Animation (Rest, (T => Animation.Time_Function (T * Rest / Animation.Duration + Duration / Animation.Duration)));
   end if;
end Drop_A;


function Last_A (Animation : Animation; Duration : Duration) return Animation is
begin
   return Drop_A (Animation, Animation.Duration - Duration);
end Last_A;


function Clamp (Lower, Upper, Value : Double) return Double is
begin
   if Lower < Upper then
      return Max (Lower, Min (Upper, Value));
   else
      return Max (Upper, Min (Lower, Value));
   end if;
end Clamp;


type Sync is (Sync_Stretch, Sync_Loop, Sync_Drop, Sync_Freeze);

function Get_Animation_Frame (Animation : Animation; Sync : Sync; Time : Time; Duration : Duration) return SVG is
begin
   case Sync is
      when Sync_Stretch => return Animation.Time_Function (Time / Duration);
      when Sync_Loop => return Animation.Time_Function (Take_Frac (Time / Animation.Duration));
      when Sync_Drop => return (if Time > Animation.Duration then None else Animation.Time_Function (Time / Animation.Duration));
      when Sync_Freeze => return Animation.Time_Function (Min (1.0, Time / Animation.Duration));
   end case;
end Get_Animation_Frame;

function Take_Frac (F : Double) return Double is
   (Int_Part, Fractional_Part : Double);
begin
   (Int_Part, Fractional_Part) := Proper_Fraction (F);
   return Fractional_Part;
end Take_Frac;
