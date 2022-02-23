unit uStringGridIME;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids
  {$ifdef MSWINDOWS}, messages {$endif};

type

  { TStringGridIME }

  TStringGridIME = class(TStringGrid)
  private
    {$ifdef MSWINDOWS}
    procedure WMIMEStartComposition(var Msg:TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMIMEComposition(var Msg:TMessage); message WM_IME_COMPOSITION;
    {$endif}
  public

  end;

procedure Register;

implementation

{$ifdef MSWINDOWS}
uses
  Windows;
{$endif}

procedure Register;
begin
  RegisterComponents('Additional',[TStringGridIME]);
end;

{ TStringGridIME }

{$ifdef MSWINDOWS}
procedure TStringGridIME.WMIMEStartComposition(var Msg: TMessage);
begin
  EditorMode:=True;
  if Assigned(InplaceEditor) then
    SendMessageW(InplaceEditor.Handle,WM_IME_STARTCOMPOSITION,Msg.wParam,Msg.lParam);
  Msg.Result:=-1;
end;

procedure TStringGridIME.WMIMEComposition(var Msg: TMessage);
begin
  if Assigned(InplaceEditor) then
    SendMessageW(InplaceEditor.Handle,WM_IME_COMPOSITION,Msg.wParam,Msg.lParam);
  Msg.Result:=-1;
end;
{$endif}

end.
