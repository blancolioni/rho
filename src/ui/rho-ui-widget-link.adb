with Css.Parser;

package body Rho.UI.Widget.Link is

   ------------
   -- Create --
   ------------

   function Create
     (Working_Path : String;
      Relation     : String;
      Href         : String)
     return Reference
   is
   begin
      if Relation = "stylesheet" then
         Css.Parser.Load_Css_File (Working_Path & "/" & Href);
      end if;

      return new Instance'
        (Parent with
           Relation => +Relation,
         Href     => +Href);
   end Create;

end Rho.UI.Widget.Link;
