-- Universal Text File (UTF) format
-- A common format for Unicode text stored as lines
--
-- Uses at most 3 bytes per code point; ASCII codes appear as themselves
-- Common definition of a line across platforms
-- Any code point may appear in a line
--
-- Copyright (C) 2022 by PragmAda Software Engineering.  All rights reserved.
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Directories;

private with Ada.Sequential_IO;

package UTF is
   type File_Handle is tagged limited private; -- with Default_Initial_Condition => not File_Handle.Is_Open;

   function Is_Open (File : in File_Handle) return Boolean;
   -- Returns True if File has been opened with Open or Create and not closed; False otherwise

   type File_Mode is (Append, Output, Input);
   subtype Output_Mode is File_Mode range Append .. Output;

   function Mode (File : in File_Handle) return File_Mode with Pre => File.Is_Open;
   -- Returns the mode that File was created/opened with

   function End_Of_File (File : in File_Handle) return Boolean with Pre => File.Is_Open and then File.Mode = Input;
   -- Returns True if File is at the end of file; False otherwise

   Invalid_File : exception;

   procedure Open (File : in out File_Handle; Name : in String; Mode : in File_Mode := Input) with
      Pre  => not File.Is_Open and Ada.Directories.Exists (Name),
      Post => File.Is_Open and File.Mode = Mode;
   -- Opens existing file Name in Mode:
   --    Append: open for output, positioned at the end of the existing file
   --    Output: open for output, truncating the existing file
   --    Input:  open for input, positioned at the beginning of the existing file
   -- Raises Invalid_File if Name exists but cannot be opened in Mode

   procedure Create (File : in out File_Handle; Name : in String; Mode : in Output_Mode := Output) with
      Pre  => not File.Is_Open,
      Post => File.Is_Open and File.Mode = Mode;
   -- If Name does not exist, creates Name for output
   -- If Name exists, same as File.Open (Name => Name, Mode => Mode)
   -- Raises Invalid_File if Name cannot be created, or exists but cannot be opened

   procedure Close (File : in out File_Handle) with
      Pre  => File.Is_Open,
      Post => not File.Is_Open;
   -- Closes File

   procedure Put_Line (To : in out File_Handle; Item : in Wide_Wide_String) with Pre => To.Is_Open and then To.Mode in Output_Mode;
   -- Writes Item to To

   EOF_Encountered : exception;

   function Next_Line (From : in out File_Handle) return Wide_Wide_String with
      Pre => From.Is_Open and then From.Mode = Input and then not From.End_Of_File;
   -- Gets a line from From
   -- Raises EOF_Encountered if an attempt is made to read past the end of file
private -- UTF
   type Byte_Value is mod 2 ** 8;

   package Byte_IO is new Ada.Sequential_IO (Element_Type => Byte_Value);

   type File_Handle is tagged limited record
      Opened    : Boolean   := False;
      Mode_Used : File_Mode := Append;
      File      : Byte_IO.File_Type;
   end record;

   function Is_Open (File : in File_Handle) return Boolean is (File.Opened);

   function Mode (File : in File_Handle) return File_Mode is (File.Mode_Used);

   function End_Of_File (File : in File_Handle) return Boolean is (Byte_IO.End_Of_File (File.File) );
end UTF;
