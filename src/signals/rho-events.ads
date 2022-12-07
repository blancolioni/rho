package Rho.Events is

   type Event_Type is
     (No_Event,
      Button_Click,
      Button_Double_Click,
      Button_Triple_Click,
      Drag_Enter,
      Drag_Leave,
      Drag_Motion,
      Drag_Status,
      Key_Press,
      Key_Release,
      Pointer_Move
     );

   type Event_Mask is array (Event_Type) of Boolean with Pack;

   type Event (Tag : Event_Type) is
      record
         null;
      end record;

end Rho.Events;
