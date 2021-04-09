with Ada.Strings.Fixed;

with WL.String_Maps;

package body Rho.Objects is

   function To_Name
     (Base  : String;
      Index : Positive)
      return String;

   package Name_Index_Maps is
     new WL.String_Maps (Positive);

   protected Names is
      procedure Get_Next_Name
        (Base : String;
         Name : out Ada.Strings.Unbounded.Unbounded_String);
   private
      Name_Index_Map : Name_Index_Maps.Map;
   end Names;

   protected body Names is

      -------------------
      -- Get_Next_Name --
      -------------------

      procedure Get_Next_Name
        (Base : String;
         Name : out Ada.Strings.Unbounded.Unbounded_String)
      is
         use Ada.Strings.Unbounded;
         use Name_Index_Maps;
         Position : constant Cursor := Name_Index_Map.Find (Base);
         Index    : Positive := 1;
      begin
         if Has_Element (Position) then
            declare
               N : Positive renames Name_Index_Map (Position);
            begin
               N := N + 1;
               Index := N;
            end;
         else
            Name_Index_Map.Insert (Base, Index);
         end if;

         Name := To_Unbounded_String (To_Name (Base, Index));
      end Get_Next_Name;

   end Names;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Root_Object_Type) is
   begin
      Names.Get_Next_Name ("object", Object.Name);
   end Initialize;

   ----------------
   -- Set_Loaded --
   ----------------

   procedure Set_Loaded (Object : in out Root_Object_Type'Class) is
   begin
      Object.Loaded := True;
   end Set_Loaded;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Object : in out Root_Object_Type'Class;
      Name   : String)
   is
   begin
      Object.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

   -------------
   -- To_Name --
   -------------

   function To_Name
     (Base  : String;
      Index : Positive)
      return String
   is
   begin
      return Base & "_"
        & Ada.Strings.Fixed.Trim (Index'Image, Ada.Strings.Left);
   end To_Name;

end Rho.Objects;
