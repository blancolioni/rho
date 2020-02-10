package body Tau.Types.Scalar is

   type Tau_Integer_Type is
     new Root_Tau_Type with null record;

   type Tau_Float_Type is
     new Root_Tau_Type with null record;

   Local_Integer_Type : aliased Tau_Integer_Type;
   Local_Float_Type   : aliased Tau_Float_Type;

   Have_Local_Types   : Boolean := False;

   procedure Check_Local_Types;

   -----------------------
   -- Check_Local_Types --
   -----------------------

   procedure Check_Local_Types is
   begin
      if not Have_Local_Types then
         Local_Integer_Type.Initialize_Object
           (GCS.Positions.Null_Position, "integer");
         Local_Float_Type.Initialize_Object
           (GCS.Positions.Null_Position, "float");
         Have_Local_Types := True;
      end if;
   end Check_Local_Types;

   ---------------
   -- Tau_Float --
   ---------------

   function Tau_Float return Tau_Type is
   begin
      Check_Local_Types;
      return Local_Float_Type'Access;
   end Tau_Float;

   -----------------
   -- Tau_Integer --
   -----------------

   function Tau_Integer return Tau_Type is
   begin
      Check_Local_Types;
      return Local_Integer_Type'Access;
   end Tau_Integer;

end Tau.Types.Scalar;
