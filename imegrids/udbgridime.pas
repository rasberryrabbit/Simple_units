unit uDBGridIME;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids
  {$ifdef MSWINDOWS}, messages {$endif};

type

  { TDBGridIME }

  TDBGridIME = class(TDBGrid)
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
  RegisterComponents('Data Controls',[TDBGridIME]);
end;

{ TDBGridIME }

{$ifdef MSWINDOWS}
procedure TDBGridIME.WMIMEStartComposition(var Msg: TMessage);
begin
  EditorMode:=True;
  if Assigned(InplaceEditor) then
    SendMessageW(InplaceEditor.Handle,WM_IME_STARTCOMPOSITION,Msg.wParam,Msg.lParam);
  Msg.Result:=-1;
end;

procedure TDBGridIME.WMIMEComposition(var Msg: TMessage);
begin
  if Assigned(InplaceEditor) then
    SendMessageW(InplaceEditor.Handle,WM_IME_COMPOSITION,Msg.wParam,Msg.lParam);
  Msg.Result:=-1;
end;
{$endif}

end.
