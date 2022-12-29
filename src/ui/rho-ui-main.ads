with Rho.Handles;

package Rho.UI.Main is

   procedure Init
     (Handle : Rho.Handles.Handle);

   function UI_Handle
     return Rho.Handles.Handle;

private

   Local_UI_Handle : Rho.Handles.Handle;

   function UI_Handle
     return Rho.Handles.Handle
   is (Local_UI_Handle);

end Rho.UI.Main;
