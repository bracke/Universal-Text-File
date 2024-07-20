-- A program to convert an Ada.Text_IO file to UTF
--
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with UTF;

procedure To_UTF is
   Input_Name : constant String := Ada.Command_Line.Argument (1);

   function Output_Name return String;
   -- Returns the appropriate output-file name, given the arguments

   function Unicode (Source : in String) return Wide_Wide_String;
   -- Converts Source to Unicode

   function Output_Name return String is
      Dot : constant Natural := Ada.Strings.Fixed.Index (Input_Name, ".", Ada.Strings.Backward);
   begin -- Output_Name
      if Ada.Command_Line.Argument_Count > 1 then
         return Ada.Command_Line.Argument (2);
      end if;

      if Dot = 0 then
         return Input_Name & ".utf";
      end if;

      return Input_Name (Input_Name'First .. Dot) & "utf";
   end Output_Name;

   function Unicode (Source : in String) return Wide_Wide_String is
      Result : Wide_Wide_String (Source'Range);
   begin -- Unicode
      Convert_All : for I in Result'Range loop
         Result (I) := Wide_Wide_Character'Val (Character'Pos (Source (I) ) );
      end loop Convert_All;

      return Result;
   end Unicode;

   Input  : Ada.Text_IO.File_Type;
   Output : UTF.File_Handle;
begin -- To_UTF
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => Input_Name);
   Output.Create (Output_Name);

   Copy_All : loop
      Output.Put_Line (Item => Unicode (Ada.Text_IO.Get_Line (Input) ) );
   end loop Copy_All;
exception -- To_UTF
when Ada.Text_IO.End_Error =>
   Ada.Text_IO.Close (File => Input);
   Output.Close;
end To_UTF;
