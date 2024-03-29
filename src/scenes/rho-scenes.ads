with Rho.Nodes;
with Rho.Render;

package Rho.Scenes is

   type Root_Scene_Type is
     new Rho.Nodes.Root_Node_Type with private;

   type Scene_Type is access all Root_Scene_Type'Class;

   function Create_Scene return Scene_Type;

private

   type Root_Scene_Type is
     new Rho.Nodes.Root_Node_Type with
      record
         null;
      end record;

   overriding function Class_Name
     (Scene : Root_Scene_Type)
      return String
   is ("scene");

   overriding procedure Load
     (Scene  : in out Root_Scene_Type;
      Target : not null access Rho.Render.Render_Target'Class);

   overriding procedure Execute_Render
     (Scene  : in out Root_Scene_Type;
      Target : not null access Rho.Render.Render_Target'Class);

end Rho.Scenes;
