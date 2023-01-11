with Rho.Logging;

package body Rho.Scenes is

   function Create_Scene return Scene_Type is
   begin
      return Scene : constant Scene_Type := new Root_Scene_Type do
         Scene.Initialize_Node;
      end return;
   end Create_Scene;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Scene  : in out Root_Scene_Type;
      Target : not null access Rho.Render.Render_Target'Class)
   is

      procedure Render_Node
        (Node : not null access Rho.Nodes.Root_Node_Type'Class);

      -----------------
      -- Render_Node --
      -----------------

      procedure Render_Node
        (Node : not null access Rho.Nodes.Root_Node_Type'Class)
      is
      begin
         Node.Before_Render (Target);
         Node.Execute_Render (Target);
         Node.After_Render (Target);
      end Render_Node;

   begin
      Scene.Iterate_Children (Render_Node'Access);
   end Execute_Render;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Scene  : in out Root_Scene_Type;
      Target :        not null access Rho.Render.Render_Target'Class)
   is

      procedure Compile_Node
        (Node : not null access Rho.Nodes.Root_Node_Type'Class);

      procedure Load_Node
        (Node : not null access Rho.Nodes.Root_Node_Type'Class);

      ------------------
      -- Compile_Node --
      ------------------

      procedure Compile_Node
        (Node : not null access Rho.Nodes.Root_Node_Type'Class)
      is
      begin
         Rho.Logging.Log ("compiling: " & Node.Class_Name & ": " & Node.Name);
         Node.Compile (Target);
      end Compile_Node;

      ---------------
      -- Load_Node --
      ---------------

      procedure Load_Node
        (Node : not null access Rho.Nodes.Root_Node_Type'Class)
      is
      begin
         Rho.Logging.Log ("loading: " & Node.Class_Name & ": " & Node.Name);
         Node.Load (Target);
      end Load_Node;

   begin
      Rho.Nodes.Root_Node_Type (Scene).Load (Target);
      Scene.Iterate_Children (Load_Node'Access);
      Scene.Iterate_Children (Compile_Node'Access);
   end Load;

end Rho.Scenes;
