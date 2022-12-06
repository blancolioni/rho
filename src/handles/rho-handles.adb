package body Rho.Handles is

   After_Render : constant Rho.Signals.Signal_Type :=
                     Rho.Signals.New_Signal ("signal-after-render");
   Before_Render : constant Rho.Signals.Signal_Type :=
                            Rho.Signals.New_Signal ("signal-before-render");

   -----------------------
   -- Set_Active_Window --
   -----------------------

   procedure Set_Active_Window
     (Handle : in out Root_Handle_Type;
      Window : Rho.Windows.Window_Type)
   is
   begin
      Handle.Active := Window;
   end Set_Active_Window;

   -------------------------
   -- Signal_After_Render --
   -------------------------

   function Signal_After_Render return Rho.Signals.Signal_Type is
   begin
      return After_Render;
   end Signal_After_Render;

   --------------------------
   -- Signal_Before_Render --
   --------------------------

   function Signal_Before_Render return Rho.Signals.Signal_Type is
   begin
      return Before_Render;
   end Signal_Before_Render;

end Rho.Handles;
