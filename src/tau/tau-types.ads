private with WL.String_Maps;

with Tau.Objects;

package Tau.Types is

   type Root_Tau_Type is
     abstract new Tau.Objects.Root_Tau_Object with private;

   type Tau_Type is access all Root_Tau_Type'Class;

   function Is_Convertible_From
     (Item : not null access Root_Tau_Type;
      From : not null access Root_Tau_Type'Class)
      return Boolean;

   function Any_Type return Tau_Type;

   function Map
     (Position : GCS.Positions.File_Position;
      From, To : Tau_Type)
      return Tau_Type;

   type Tau_Type_Array is array (Positive range <>) of Tau_Type;

   function Map
     (Position : GCS.Positions.File_Position;
      From     : Tau_Type_Array;
      To       : Tau_Type)
      return Tau_Type;

   function Is_Convertible_From_Vector
     (Item : not null access Root_Tau_Type;
      From : Tau_Type_Array)
      return Boolean;

   function Map_From
     (Item : Root_Tau_Type)
      return Tau_Type_Array;

   function Map_To
     (Item : not null access Root_Tau_Type)
      return Tau_Type;

   function Has_Property
     (Item : Root_Tau_Type;
      Name : String)
     return Boolean;

   function Property_Type
     (Item : Root_Tau_Type;
      Name : String)
      return Tau_Type
     with Pre'Class => Item.Has_Property (Name);

   function Has_Uniform_Binding
     (Item : Root_Tau_Type)
      return Boolean;

private

   package Property_Maps is
     new WL.String_Maps (Tau_Type);

   type Root_Tau_Type is
     abstract new Tau.Objects.Root_Tau_Object with
      record
         Properties : Property_Maps.Map;
      end record;

   overriding function Class_Name
     (Typ : Root_Tau_Type)
      return String
   is ("type");

   function Has_Property
     (Item : Root_Tau_Type;
      Name : String)
      return Boolean
   is (Item.Properties.Contains (Name));

   function Property_Type
     (Item : Root_Tau_Type;
      Name : String)
      return Tau_Type
   is (Item.Properties.Element (Name));

   function Has_Uniform_Binding
     (Item : Root_Tau_Type)
      return Boolean
   is (False);

end Tau.Types;
