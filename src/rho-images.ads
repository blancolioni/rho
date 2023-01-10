with WL.Generic_Real_Images;

package Rho.Images is

   package Real_Images is new WL.Generic_Real_Images (Real);

   function Image (X : Real) return String
                   renames Real_Images.Approximate_Image;

end Rho.Images;
