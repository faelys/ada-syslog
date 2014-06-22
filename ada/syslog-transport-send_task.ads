------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Syslog.Transport.Send_Task provides a transporter that adds messages     --
-- to a protected queue, so that they can be sent sequentially from a       --
-- dedicated task, thereby providing a task-safe layer on top a             --
-- non-reentrant transporter.                                               --
------------------------------------------------------------------------------

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Syslog.Transport.Send_Task is

   Transport : constant Transporter;

   procedure Set_Backend (Send : in Transporter);

private

   Backend : Transporter := null;


   package Message_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   protected Queue is
      entry Append
        (Message : in Ada.Strings.Unbounded.Unbounded_String;
         Send : in not null Transporter);
      procedure Get
        (Message : out Ada.Strings.Unbounded.Unbounded_String;
         Send : out Transporter);
   private
      List : Message_Lists.List;
      Task_Waiting : Boolean := False;
   end Queue;


   task Sender is
      entry Send
        (Message : in Ada.Strings.Unbounded.Unbounded_String;
         Transport : in not null Transporter);
   end Sender;


   procedure Local_Transport (Message : in String);

   Transport : constant Transporter := Local_Transport'Access;

end Syslog.Transport.Send_Task;
