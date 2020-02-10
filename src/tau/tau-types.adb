with Ada.Strings.Unbounded;

with Tau.Types.Maps;

package body Tau.Types is

   type Any_Tau_Type is new Root_Tau_Type with null record;

   overriding function Is_Convertible_From
     (Any : not null access Any_Tau_Type;
      From : not null access Root_Tau_Type'Class)
      return Boolean;

   overriding function Is_Convertible_From_Vector
     (Item : not null access Any_Tau_Type;
      From : Tau_Type_Array)
      return Boolean;

   Have_Any_Type  : Boolean := False;
   Local_Any_Type : aliased Any_Tau_Type;

   --------------
   -- Any_Type --
   --------------

   function Any_Type return Tau_Type is
   begin
      if not Have_Any_Type then
         Local_Any_Type.Initialize_Object
           (GCS.Positions.Null_Position, "any");
         Have_Any_Type := True;
      end if;
      return Local_Any_Type'Access;
   end Any_Type;

   -------------------------
   -- Is_Convertible_From --
   -------------------------

   function Is_Convertible_From
     (Item : not null access Root_Tau_Type;
      From : not null access Root_Tau_Type'Class)
      return Boolean
   is
   begin
      return Tau_Type (Item) = Tau_Type (From);
   end Is_Convertible_From;

   -------------------------
   -- Is_Convertible_From --
   -------------------------

   overriding function Is_Convertible_From
     (Any  : not null access Any_Tau_Type;
      From : not null access Root_Tau_Type'Class)
      return Boolean
   is
      pragma Unreferenced (Any, From);
   begin
      return True;
   end Is_Convertible_From;

   --------------------------------
   -- Is_Convertible_From_Vector --
   --------------------------------

   overriding function Is_Convertible_From_Vector
     (Item : not null access Any_Tau_Type;
      From : Tau_Type_Array)
      return Boolean
   is
      pragma Unreferenced (Item, From);
   begin
      return True;
   end Is_Convertible_From_Vector;

   --------------------------------
   -- Is_Convertible_From_Vector --
   --------------------------------

   function Is_Convertible_From_Vector
     (Item : not null access Root_Tau_Type;
      From : Tau_Type_Array)
      return Boolean
   is
      pragma Unreferenced (Item, From);
   begin
      return False;
   end Is_Convertible_From_Vector;

   ---------
   -- Map --
   ---------

   function Map
     (Position : GCS.Positions.File_Position;
      From, To : Tau_Type)
      return Tau_Type
   is
   begin
      return Map (Position, (1 => From), To);
   end Map;

   ---------
   -- Map --
   ---------

   function Map
     (Position : GCS.Positions.File_Position;
      From     : Tau_Type_Array;
      To       : Tau_Type)
      return Tau_Type
   is
      use Ada.Strings.Unbounded;
      Name : Unbounded_String;
      Result : Tau.Types.Maps.Map_Type :=
        Tau.Types.Maps.Map_Type'
          (Root_Tau_Type with
           Arity => From'Length,
           From  => From,
           To    => To);
   begin
      for I in From'Range loop
         if I > From'First then
            Name := Name & "x";
         end if;
         Name := Name & From (I).Name;
      end loop;
      Name := Name & "->" & To.Name;
      Result.Initialize_Object (Position, To_String (Name));

      return new Tau.Types.Maps.Map_Type'(Result);
   end Map;

   --------------
   -- Map_From --
   --------------

   function Map_From
     (Item : Root_Tau_Type)
      return Tau_Type_Array
   is
      pragma Unreferenced (Item);
   begin
      return From : Tau_Type_Array (1 .. 0);
   end Map_From;

   ------------
   -- Map_To --
   ------------

   function Map_To
     (Item : not null access Root_Tau_Type)
      return Tau_Type
   is
   begin
      return Tau_Type (Item);
   end Map_To;

end Tau.Types;
