module type S = sig
  module Auth : Auth_service.S
end
