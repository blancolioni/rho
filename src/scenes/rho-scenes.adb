with Rho.Logging;

package body Rho.Scenes is

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
         Rho.Logging.Log ("compiling: " & Node.Name);
         Node.Compile (Target);
      end Compile_Node;

      ---------------
      -- Load_Node --
      ---------------

      procedure Load_Node
        (Node : not null access Rho.Nodes.Root_Node_Type'Class)
      is
      begin
         Rho.Logging.Log ("loading: " & Node.Name);
         Node.Load (Target);
      end Load_Node;

   begin
      Rho.Nodes.Root_Node_Type (Scene).Load (Target);
      Scene.Iterate_Children (Load_Node'Access);
      Scene.Iterate_Children (Compile_Node'Access);
   end Load;

end Rho.Scenes;
