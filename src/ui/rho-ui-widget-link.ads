package Rho.UI.Widget.Link is

   subtype Parent is Widget.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Create
     (Working_Path : String;
      Relation     : String;
      Href         : String)
      return Reference;

private

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         Relation : Rho.Strings.Rho_String;
         Href     : Rho.Strings.Rho_String;
      end record;

   overriding function Class_Name
     (This : Instance)
      return String
   is ("Rho.UI.Widget.Link");

end Rho.UI.Widget.Link;
