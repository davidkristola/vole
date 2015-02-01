
with kv.avm.Clients;

package kv.avm.Servers is

   type Server_Interface is interface;
   type Server_Access is access all Server_Interface'CLASS;

   function Is_Connection_Waiting(Self : Server_Interface) return Boolean is abstract;
   function Get_Connection(Self : Server_Interface) return kv.avm.Clients.Client_Access is abstract;

end kv.avm.Servers;
