-- A program to convert a UTF file into an Ada.Text_IO file
--
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with UTF;

procedure From_UTF is
   Input_Name : constant String := Ada.Command_Line.Argument (1);

   function Output_Name return String;
   -- Returns the appropriate output-file name, given the arguments

   function Latin (Source : in Wide_Wide_String) return String;
   -- Converts Source to Latin-1

   function Output_Name return String is
      Dot : constant Natural := Ada.Strings.Fixed.Index (Input_Name, ".", Ada.Strings.Backward);
   begin -- Output_Name
      if Ada.Command_Line.Argument_Count > 1 then
         return Ada.Command_Line.Argument (2);
      end if;

      if Dot = 0 then
         return Input_Name & ".txt";
      end if;

      return Input_Name (Input_Name'First .. Dot) & "txt";
   end Output_Name;

   function Latin (Source : in Wide_Wide_String) return String is
      Result : String (Source'Range);
   begin -- Latin
      Convert_All : for I in Result'Range loop
         Result (I) := Character'Val (Wide_Wide_Character'Pos (Source (I) ) );
      end loop Convert_All;

      return Result;
   end Latin;

   Input  : UTF.File_Handle;
   Output : Ada.Text_IO.File_Type;
begin -- From_UTF
   Input.Open (Name => Input_Name, Mode => UTF.Input);
   Ada.Text_IO.Create (File => Output, Name => Output_Name);

   Copy_All : loop
      exit Copy_All when Input.End_Of_File;

      Ada.Text_IO.Put_Line (File => Output, Item => Latin (Input.Next_Line) );
   end loop Copy_All;

   Input.Close;
   Ada.Text_IO.Close (File => Output);
end From_UTF;
