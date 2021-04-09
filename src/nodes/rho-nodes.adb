with WL.Guids;

with Rho.Matrices.Logs;

package body Rho.Nodes is

   ---------
   -- Add --
   ---------

   procedure Add
     (Node : not null access Root_Node_Type;
      Child  : not null access Root_Node_Type'Class)
   is
   begin
      if Node.Same_As (Child) then
         raise Constraint_Error with
           "cannot add " & Node.Show & " to itself";
      end if;

      if Child.Parent /= null then
         Child.Parent.Remove (Child);
      end if;

      Child.Parent := Node_Type (Node);
      Node.Children.Append (Node_Type (Child));
   end Add;

   ------------------
   -- After_Render --
   ------------------

   overriding procedure After_Render
     (Node     : in out Root_Node_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin
      for Child of Node.Children loop
         Child.After_Render (Target);
      end loop;
   end After_Render;

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Node   : in out Root_Node_Type;
      Target : not null access Rho.Render.Render_Target'Class)
   is
   begin
      if not Node.Is_Loaded then
         Root_Node_Type'Class (Node).Load (Target);
      end if;

      if Node.Local_Out_Of_Date then
         Root_Node_Type'Class (Node).Update_Matrix;
      end if;

      for Child of Node.Children loop
         Child.Before_Render (Target);
      end loop;

   end Before_Render;

   -------------------
   -- Create_Node --
   -------------------

   function Create_Node return Node_Type is
   begin
      return new Root_Node_Type;
   end Create_Node;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Node     : in out Root_Node_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin
--        Rho.Matrices.Logs.Log_Matrix
--          (Node.Show,
--           Node.Local_Matrix);

      Target.Set_Model_View_Matrix (Node.World_Matrix);

      for Child of Node.Children loop
         Child.Execute_Render (Target);
      end loop;

   end Execute_Render;

   -----------------------------
   -- Invalidate_World_Matrix --
   -----------------------------

   procedure Invalidate_World_Matrix
     (Node : in out Root_Node_Type'Class)
   is
   begin
      Node.World_Out_Of_Date := True;
      for Child of Node.Children loop
         Child.Invalidate_World_Matrix;
      end loop;
   end Invalidate_World_Matrix;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Root : in out Root_Node_Type'Class;
      Process : not null access
        procedure (Node : in out Root_Node_Type'Class))
   is
   begin
      Process (Root);
      for Child of Root.Children loop
         Child.Iterate (Process);
      end loop;
   end Iterate;

   ----------------------
   -- Iterate_Children --
   ----------------------

   procedure Iterate_Children
     (Root : in out Root_Node_Type'Class;
      Process : not null access
        procedure (Node : not null access Root_Node_Type'Class))
   is
   begin
      for Child of Root.Children loop
         Process (Child);
         Child.Iterate_Children (Process);
      end loop;
   end Iterate_Children;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Node     : in out Root_Node_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
      pragma Unreferenced (Target);
   begin
      Node.Set_Loaded;
   end Load;

   ------------------
   -- Local_Matrix --
   ------------------

   function Local_Matrix
     (Node : in out Root_Node_Type'Class)
      return Rho.Matrices.Matrix_4
   is
   begin
      if Node.Local_Out_Of_Date then
         Node.Update_Matrix;
      end if;
      return Node.M_Local;
   end Local_Matrix;

   ---------------------------
   -- On_Quaternion_Changed --
   ---------------------------

   procedure On_Quaternion_Changed
     (Node : in out Root_Node_Type)
   is
   begin
      Node.Local_Out_Of_Date := True;
      Root_Node_Type'Class (Node).Invalidate_World_Matrix;
   end On_Quaternion_Changed;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Node : not null access Root_Node_Type;
      Child  : not null access Root_Node_Type'Class)
   is
      Position : Node_Lists.Cursor :=
        Node.Children.Find (Node_Type (Child));
   begin
      if Child.Parent = null then
         raise Constraint_Error with
           "cannot delete child " & Child.Show & " from " & Node.Show
           & " because it has no parent";
      end if;

      if not Child.Parent.Same_As (Node) then
         raise Constraint_Error with
           "cannot delete child " & Child.Show & " from " & Node.Show
           & " because its parent is " & Child.Parent.Show;
      end if;

      if not Node_Lists.Has_Element (Position) then
         raise Constraint_Error with
         Child.Show & " thinks its parent is " & Node.Show
           & " but it does not appear in the child list";
      end if;

      Node.Children.Delete (Position);
      Child.Parent := null;
   end Remove;

   --------------------
   -- Rotate_On_Axis --
   --------------------

   procedure Rotate_On_Axis
     (Node  : in out Root_Node_Type'Class;
      Axis  : Rho.Matrices.Normal_Vector_3;
      Angle : Real)
   is
      use Rho.Matrices;
      Q : constant Quaternion :=
        Axis_Angle_Quaternion (Axis, Angle);
   begin
      Node.Quaternion := Node.Quaternion * Q;
      Node.On_Quaternion_Changed;
   end Rotate_On_Axis;

   --------------
   -- Rotate_X --
   --------------

   procedure Rotate_X
     (Node  : in out Root_Node_Type'Class;
      Angle : Real)
   is
   begin
      Node.Rotate_On_Axis
        (Axis  => Rho.Matrices.Unit_X,
         Angle => Angle);
   end Rotate_X;

   --------------
   -- Rotate_Y --
   --------------

   procedure Rotate_Y
     (Node  : in out Root_Node_Type'Class;
      Angle : Real)
   is
   begin
      Node.Rotate_On_Axis
        (Axis  => Rho.Matrices.Unit_Y,
         Angle => Angle);
   end Rotate_Y;

   --------------
   -- Rotate_Z --
   --------------

   procedure Rotate_Z
     (Node  : in out Root_Node_Type'Class;
      Angle : Real)
   is
   begin
      Node.Rotate_On_Axis
        (Axis  => Rho.Matrices.Unit_Z,
         Angle => Angle);
   end Rotate_Z;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Node  : in out Root_Node_Type'Class;
      X, Y, Z : Real)
   is
   begin
      Node.Position := Rho.Matrices.To_Vector (X, Y, Z);
      Node.Local_Out_Of_Date := True;
      Node.Invalidate_World_Matrix;
   end Set_Position;

   ----------------------
   -- Set_World_Matrix --
   ----------------------

   procedure Set_World_Matrix
     (Node : in out Root_Node_Type'Class;
      Matrix : Rho.Matrices.Matrix_4)
   is
   begin
      Node.Invalidate_World_Matrix;
      Node.M_World := Matrix;
      Node.World_Out_Of_Date := False;
   end Set_World_Matrix;

   ----------------------
   -- Shader_Slices --
   ----------------------

   function Shader_Slices
     (Node : Root_Node_Type)
      return Rho.Shaders.Slices.Slice_Array
   is
   begin
      return Result : Rho.Shaders.Slices.Slice_Array (1 .. 0);
   end Shader_Slices;

   ----------
   -- Show --
   ----------

   function Show (Node : Root_Node_Type) return String is
   begin
      if Node.Name = "" then
         return WL.Guids.To_String (Node.Guid);
      else
         return Node.Name;
      end if;
   end Show;

   -------------------
   -- Update_Matrix --
   -------------------

   procedure Update_Matrix
     (Node : in out Root_Node_Type)
   is
   begin
      Node.M_Local :=
        Rho.Matrices.Compose (Node.Position, Node.Quaternion,
                              Node.Scale);

      if False then
         Rho.Matrices.Logs.Log_State
           (Node.Name, Node.Position, Node.Quaternion, Node.Scale,
            Node.M_Local);
      end if;
      Node.Local_Out_Of_Date := False;
   end Update_Matrix;

   -------------------------
   -- Update_World_Matrix --
   -------------------------

   procedure Update_World_Matrix
     (Node : in out Root_Node_Type)
   is
      use type Rho.Matrices.Matrix_4;
   begin
      if Node.Local_Out_Of_Date then
         Node.Update_Matrix;
      end if;

      if Node.Parent /= null then
         Node.M_World :=
           Node.Parent.World_Matrix * Node.M_Local;
      else
         Node.M_World := Node.M_Local;
      end if;

      Node.World_Out_Of_Date := False;

   end Update_World_Matrix;

   ------------------
   -- World_Matrix --
   ------------------

   function World_Matrix
     (Node : in out Root_Node_Type'Class)
      return Rho.Matrices.Matrix_4
   is
   begin
      if Node.World_Out_Of_Date then
         Node.Update_World_Matrix;
      end if;
      return Node.M_World;
   end World_Matrix;

end Rho.Nodes;
