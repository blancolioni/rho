package Rho.UI.Widget.Main_Root is

   subtype Parent is Widget.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Create
     return Reference;

private

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         null;
      end record;

   overriding procedure Configure
     (This : not null access Instance);

   overriding procedure Map
     (This : not null access Instance;
      Surface : not null access
        Rho.UI.Surface.Instance'Class);

   overriding procedure Show
     (This   : not null access Instance;
      Target : not null access Rho.Render.Render_Target'Class);

   overriding function Class_Name
     (This : Instance)
      return String
   is ("Rho.UI.Widget.Main_Root");

end Rho.UI.Widget.Main_Root;
