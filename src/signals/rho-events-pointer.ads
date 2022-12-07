package Rho.Events.Pointer is

   type Pointer_Event_Type is new Root_Event_Type with private;

private

   type Pointer_Event_Type is new Root_Event_Type with
      record
         X, Y : Real;
      end record;

end Rho.Events.Pointer;
