module type S = sig
  module Auth : Auth_service.S
  module Db : Database_service.S
end
