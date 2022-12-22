with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Blender (Script : Unbounded_String) is
   exec : constant String := "blender";
   py_file : String := "temp.py";
begin
   Ada.Text_IO.Put_Line (Script);
   Ada.Text_IO.Create (py_file);
   Ada.Text_IO.Put (Script, py_file);
   Ada.Text_IO.Close (py_file);
   Ada.Command_Line.System
     (exec & " --background --render-format PNG --python-exit-code 1 --render-output temp.png --python " & py_file);
end Blender;
