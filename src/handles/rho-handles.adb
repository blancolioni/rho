with Ada.Containers.Indefinite_Holders;

package body Rho.Handles is

   package Signal_Holders is
     new Ada.Containers.Indefinite_Holders
       (Rho.Signals.Signal_Type, Rho.Signals."=");

   Before_Render_Holder : Signal_Holders.Holder;
   After_Render_Holder  : Signal_Holders.Holder;

   function Get_Signal
     (Holder : in out Signal_Holders.Holder;
      Name   : String)
      return Rho.Signals.Signal_Type;

   ----------------
   -- Get_Signal --
   ----------------

   function Get_Signal
     (Holder : in out Signal_Holders.Holder;
      Name   : String)
      return Rho.Signals.Signal_Type
   is
   begin
      if Holder.Is_Empty then
         Holder := Signal_Holders.To_Holder (Rho.Signals.New_Signal (Name));
      end if;

      pragma Assert (Rho.Signals.Signal_Name (Holder.Element) = Name);
      return Holder.Element;
   end Get_Signal;

   -------------------------
   -- Signal_After_Render --
   -------------------------

   function Signal_After_Render return Rho.Signals.Signal_Type is
   begin
      return Get_Signal (After_Render_Holder, "signal-after-render");
   end Signal_After_Render;

   --------------------------
   -- Signal_Before_Render --
   --------------------------

   function Signal_Before_Render return Rho.Signals.Signal_Type is
   begin
      return Get_Signal (Before_Render_Holder, "signal-before-render");
   end Signal_Before_Render;

end Rho.Handles;
