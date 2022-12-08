package body Rho.UI.Widget is

   overriding function Tag (This : Instance) return String
   is (-This.Tag);

   overriding function Id  (This : Instance) return String
   is (-This.Id);

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (This  : not null access Instance;
      Child : not null access Instance'Class)
   is
   begin
      This.Children.Append (Reference (Child));
   end Add_Child;

   ------------------
   -- Create_Style --
   ------------------

   overriding procedure Create_Style
     (This   : in out Instance;
      Name   : String)
   is
   begin
      This.Style_Map.Create_Style (Name);
   end Create_Style;

   ----------
   -- Find --
   ----------

   function Find
     (Top   : not null access Instance'Class;
      Match : not null access function (This : Reference) return Boolean)
      return Reference
   is
   begin
      if Match (Reference (Top)) then
         return Reference (Top);
      else
         for Child of Top.Children loop
            declare
               Result : constant Reference := Child.Find (Match);
            begin
               if Result /= null then
                  return Result;
               end if;
            end;
         end loop;
         return null;
      end if;
   end Find;

   ----------------
   -- Find_By_Id --
   ----------------

   function Find_By_Id
     (Top : not null access Instance'Class;
      Id  : String)
      return Reference
   is
      function Match_Id (W : Reference) return Boolean
      is (-W.Id = Id);

   begin
      return Top.Find (Match_Id'Access);
   end Find_By_Id;

   -------------------------
   -- Get_Layout_Position --
   -------------------------

   overriding function Get_Layout_Position
     (This : Instance)
      return Css.Layout_Position
   is
   begin
      return This.Position;
   end Get_Layout_Position;

   ---------------------
   -- Get_Layout_Size --
   ---------------------

   overriding function Get_Layout_Size
     (This : Instance)
      return Css.Layout_Size
   is
   begin
      return This.Size;
   end Get_Layout_Size;

   ------------------
   -- On_Configure --
   ------------------

   procedure On_Configure
     (This    : in out Instance'Class;
      Handler :        not null access function
        (This : not null access Instance'Class; Width, Height : Natural)
         return Boolean)
   is null;

   -------------
   -- On_Draw --
   -------------

   procedure On_Draw
     (This    : in out Instance'Class;
      Handler :        not null access function
        (This : not null access Instance'Class; Cr : Cairo.Cairo_Context)
         return Boolean)
   is null;

   -------------------------
   -- Set_Layout_Position --
   -------------------------

   overriding procedure Set_Layout_Position
     (This     : in out Instance;
      Position : Css.Layout_Position)
   is
   begin
      This.Position := Position;
   end Set_Layout_Position;

   ---------------------
   -- Set_Layout_Size --
   ---------------------

   overriding procedure Set_Layout_Size
     (This   : in out Instance;
      Size   : Css.Layout_Size)
   is
   begin
      This.Size := Size;
   end Set_Layout_Size;

   ---------------
   -- Set_Style --
   ---------------

   overriding procedure Set_Style
     (This   : in out Instance;
      Name   : String;
      State  : String;
      Value  : Css.Css_Element_Value)
   is
   begin
      This.Style_Map.Set_Style (Name, State, Value);
   end Set_Style;

   -----------
   -- Style --
   -----------

   overriding function Style
     (This : Instance;
      Name : String)
      return Css.Css_Element_Value
   is
   begin
      return This.Style_Map.Style (Name);
   end Style;

end Rho.UI.Widget;
