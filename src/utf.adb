-- Universal Text File (UTF) format
--
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Exceptions;
with System;

package body UTF is
   function Byte_Mode (Mode : in File_Mode) return Byte_IO.File_Mode is
      (case Mode is
       when Append => Byte_IO.Append_File,
       when Output => Byte_IO.Out_File,
       when Input  => Byte_IO.In_File);

   use type Byte_IO.File_Mode;

   Base : constant := 128;

   -- Numbers in UTF are encoded as base-Base digits in little-endian format
   -- All bytes except the one containing the MSD have their MSBs set to one (= digit + Base),
   -- so ASCII characters are encoded as themselves
   -- Since Base ** 3 = 2 ** 21, all Unicode characters can be encoded in 3 bytes

   type Big is mod System.Max_Binary_Modulus;

   function Read (From : in Byte_IO.File_Type) return Big with
      Pre => Byte_IO.Is_Open (From) and then Byte_IO.Mode (From) = Byte_IO.In_File;
   -- Reads an encoded value from From

   procedure Write (To : in Byte_IO.File_Type; Value : in Big) with
      Pre => Byte_IO.Is_Open (To) and then Byte_IO.Mode (To) in Byte_IO.Append_File | Byte_IO.Out_File;
   -- Writes Value to To encoded as a sequence of base-Base digits

   procedure Open (File : in out File_Handle; Name : in String; Mode : in File_Mode := Input) is
      -- Empty
   begin -- Open
      Byte_IO.Open (File => File.File, Mode => Byte_Mode (Mode), Name => Name);
      File.Opened := True;
      File.Mode_Used := Mode;
   exception -- Open
   when E : Byte_IO.Use_Error =>
      raise Invalid_File with Ada.Exceptions.Exception_Message (E);
   end Open;

   procedure Create (File : in out File_Handle; Name : in String; Mode : in Output_Mode := Output) is
      -- Empty
   begin -- Create
      if Ada.Directories.Exists (Name) then
         Byte_IO.Open (File => File.File, Mode => Byte_Mode (Mode), Name => Name);
      else
         Byte_IO.Create (File => File.File, Name => Name);
      end if;

      File.Opened := True;
      File.Mode_Used := Mode;
   exception -- Create
   when E : Byte_IO.Use_Error =>
      raise Invalid_File with Ada.Exceptions.Exception_Message (E);
   end Create;

   procedure Close (File : in out File_Handle) is
      -- Empty
   begin -- Close
      Byte_IO.Close (File => File.File);
      File.Opened := False;
   end Close;

   procedure Put_Line (To : in out File_Handle; Item : in Wide_Wide_String) is
      -- Empty
   begin -- Put_Line
      Write (To => To.File, Value => Item'Length);

      All_Characters : for C of Item loop
         Write (To => To.File, Value => Wide_Wide_Character'Pos (C) );
      end loop All_Characters;
   end Put_Line;

   function Next_Line (From : in out File_Handle) return Wide_Wide_String is
      -- Empty
   begin -- Next_Line
      Create_Line : declare
         Result : Wide_Wide_String (1 .. Natural (Read (From.File) ) );
      begin -- Create_Line
         Read_All : for I in Result'Range loop
            Result (I) := Wide_Wide_Character'Val (Read (From.File) );
         end loop Read_All;

         return Result;
      end Create_Line;
   exception -- Next_Line
   when E : Byte_IO.End_Error =>
      raise EOF_Encountered with Ada.Exceptions.Exception_Message (E);
   end Next_Line;

   function Read (From : in Byte_IO.File_Type) return Big is
      Result : Big := 0;
      Byte   : Byte_Value;
      Mult   : Big := 1;
   begin -- Read
      All_Digits : loop
         Byte_IO.Read (File => From, Item => Byte);
         Result := Result + Mult * Big (if Byte < Base then Byte else Byte - Base);
         Mult := Base * Mult;

         exit All_Digits when Byte < Base;
      end loop All_Digits;

      return Result;
   end Read;

   procedure Write (To : in Byte_IO.File_Type; Value : in Big) is
      Source : Big := Value;
      Byte   : Byte_Value;
   begin -- Write
      All_Digits : loop
         Byte := Byte_Value (Source rem Base) + (if Source < Base then 0 else Base);
         Byte_IO.Write (File => To, Item => Byte);

         exit All_Digits when Source < Base;

         Source := Source / Base;
      end loop All_Digits;
   end Write;
end UTF;
