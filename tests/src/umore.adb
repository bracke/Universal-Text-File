-- A simple "more" program for UTF files
--
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Command_Line;
with Ada.Containers.Indefinite_Holders;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Text_IO;
with UTF;

procedure Umore is
   Available_Lines   : constant := 23;
   Available_Columns : constant := 80;

   function Lines_Needed (Line : in Wide_Wide_String) return Positive is
      (if Line'Length = 0 then 1 else Line'Length / Available_Columns + (if Line'Length rem Available_Columns = 0 then 0 else 1) );

   package Line_Holders is new Ada.Containers.Indefinite_Holders (Element_Type => Wide_Wide_String);

   File         : UTF.File_Handle;
   Lines_Shown  : Natural;
   Buffer       : Line_Holders.Holder;
begin -- Umore
   if Ada.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line (Item => "Usage: umore <file name>");

      return;
   end if;

   File.Open (Name => Ada.Command_Line.Argument (1) );

   All_Pages : loop
      exit All_Pages when File.End_Of_File and Buffer.Is_Empty;

      Lines_Shown := 0;

      if not Buffer.Is_Empty then
         Held_Line : declare
            Line : constant Wide_Wide_String := Buffer.Element;
         begin -- Held_Line
            Ada.Text_IO.Put_Line (Item => Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Line) );
            Lines_Shown := Lines_Needed (Line);
            Buffer.Clear;
         end Held_Line;
      end if;

      One_Page : loop
         exit One_Page when File.End_Of_File;

         One_Line : declare
            Line : constant Wide_Wide_String := File.Next_Line;
         begin -- One_Line
            if Lines_Shown + Lines_Needed (Line) > Available_Lines then
               Buffer := Line_Holders.To_Holder (Line);

               exit One_Page;
            else
               Ada.Text_IO.Put_Line (Item => Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Line) );
               Lines_Shown := Lines_Shown + Lines_Needed (Line);
            end if;
         end One_Line;
      end loop One_Page;

      Ada.Text_IO.Skip_Line;
   end loop All_Pages;
end Umore;
