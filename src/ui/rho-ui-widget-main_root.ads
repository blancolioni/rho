with Rho.Scenes;

package Rho.UI.Widget.Main_Root is

   subtype Parent is Widget.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   function Create
     return Reference;

   function Root_Node
     (This : Instance)
      return Rho.Nodes.Node_Type;

private

   subtype Dispatch is Parent'Class;

   type Instance is new Parent with
      record
         Scene : Rho.Scenes.Scene_Type;
      end record;

   overriding procedure Map
     (This : not null access Instance;
      Surface : not null access constant
        Rho.Rectangles.Rectangle_Interface'Class);

   function Root_Node
     (This : Instance)
      return Rho.Nodes.Node_Type
   is (Rho.Nodes.Node_Type (This.Scene));

end Rho.UI.Widget.Main_Root;
