unit ulabelbox;

{$mode objfpc}{$H+}

{ Simple LabelBox

  Copyright (c) 2017 Do-wan Kim

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}


interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Graphics, Forms;

type

  { TLabelBox }

  TLabelBox = class(TWinControl)
    private
      fLabel: TLabel;
      fShape: TShape;
      fEditor: TEdit;
      fToggle: Boolean;
      fMarginX, fMarginY: Integer;
      fAutoFit:Boolean;
      fColorMark, fColorUnMark: TColor;
      fLWidthNormal, fLWidthFocus: Integer;
      fOldCaption: TCaption;
      fOnLabelChanged:TNotifyEvent;

      procedure SetLabelCaption(Value:TCaption);
      function GetLabelCaption:TCaption;
      procedure OnEditorExit(Sender:TObject);
      procedure OnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure SetColorMark(Value:TColor);
      procedure SetColorUnMark(Value:TColor);
      procedure SetLWidthNormal(Value:Integer);
      procedure SetLWidthFocus(Value:Integer);
      procedure SetToggle(Value:Boolean);
      procedure SetMarginX(Value:Integer);
      procedure SetMarginY(Value:Integer);
      procedure SetAutoFit(Value:Boolean);
    protected
      procedure Click; override;
      procedure DblClick; override;
      procedure DoEnter; override;
      procedure DoExit; override;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure OnClickEvent(Sender:TObject);
      procedure OnDblClickEvent(Sender:TObject);
    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;

      procedure UpdateLabel; virtual;

      property LabelControl:TLabel read fLabel;
      property ShapeControl:TShape read fShape;
      property Caption:TCaption read GetLabelCaption write SetLabelCaption;
      property ColorMark:TColor read fColorMark write SetColorMark;
      property ColorUnMark:TColor read fColorUnMark write SetColorUnMark;
      property LWidthNormal:Integer read fLWidthNormal write SetLWidthNormal;
      property LWidthFocus:Integer read fLWidthFocus write SetLWidthFocus;
      property Toggle:Boolean read fToggle write SetToggle;
      property MarginX:Integer read fMarginX write SetMarginX;
      property MarginY:Integer read fMarginY write SetMarginY;
      property AutoFit:Boolean read fAutoFit write SetAutoFit;
      property OnLabelChanged:TNotifyEvent read fOnLabelChanged write fOnLabelChanged;
    published
      property Left;
      property Top;
      property TabStop;
  end;

  { TLabelBoxArray }

  TLabelBoxArray = class(TScrollBox)
    private
      fHGap,fVGap: Integer;
      fLabelBound: TRect;
      fLabelCount: Integer;
      fLabelMarginX, fLabelMarginY: Integer;
      fLColorMark, fLColorUnMark: TColor;
      fOnLabelClick: TNotifyEvent;
      fOnLabelDblClick: TNotifyEvent;

      procedure SetLabelBound(const Value:TRect);
      procedure SetLabelHGap(Value:Integer);
      procedure SetLabelVGap(Value:Integer);
      procedure OnArrayLabelChanged(Sender:TObject);
      function GetMarked(Idx:Integer):Boolean;
      procedure SetMarked(Idx:Integer;Value:Boolean);
      function GetLabelBox(Idx:Integer):TLabelBox;
      function GetLabelBoxName(const LName:TCaption):TLabelBox;
    protected
      procedure BoundsChanged; override;
    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      // function and procedure
      procedure AddLabel(const LName: TCaption);
      procedure DeleteLabel(Idx:Integer);
      procedure DeleteLabel(const sLabel:TCaption); overload;
      procedure DeleteLabel(Marked:Boolean); overload;
      procedure InsertLabel(Idx:Integer; const LName:TCaption);
      procedure ReArrangeLabels;
      procedure Clear;
      // default Label click and dblclick handler
      procedure OnLabelClickEvent(Sender:TObject);
      procedure OnLabelDblClickEvent(Sender:TObject);

      property LabelBound:TRect read fLabelBound write SetLabelBound;
      property LabelHGap:Integer read fHGap write SetLabelHGap;
      property LabelVGap:Integer read fVGap write SetLabelVGap;
      property LabelCount:Integer read fLabelCount;
      property LabelMarginX:Integer read fLabelMarginX write fLabelMarginX;
      property LabelMarginY:Integer read fLabelMarginY write fLabelMarginY;
      property LabelBox[Index:Integer]:TLabelBox read GetLabelBox;
      property LabelBoxName[LName:TCaption]:TLabelBox read GetLabelBoxName;
      property Marked[Index:Integer]:Boolean read GetMarked write SetMarked;
      property LColorMark:TColor read fLColorMark write fLColorMark;
      property LColorUnMark:TColor read fLColorUnMark write fLColorUnMark;
      property OnLabelClick:TNotifyEvent read fOnLabelClick write fOnLabelClick;
      property OnLabelDblClick:TNotifyEvent read fOnLabelDblClick write fOnLabelDblClick;
    published
      property Width;
      property Height;
      property Top;
      property Left;
  end;

  { TLabelBoxLine }

  TLabelBoxLine = class(TLabelBox)
    private
      fLineShape:TShape;
      fLineDrection:Integer;      // 0=down, 1=up, 2=right, 3=left
      fLineLength:Integer;
      fLineWidth:Integer;
      fLineMarkColor,
      fLineUnMarkColor:TColor;
      fAfterLineColorUpdate:TNotifyEvent;

      procedure SetLineDirection(Value:Integer);
      procedure SetLineLength(Value:Integer);
      procedure SetLineWidth(Value:Integer);
      procedure SetLineMarkColor(Value:TColor);
      procedure SetLineUnMarkColor(Value:TColor);
      procedure UpdateLineColor;
    protected
      procedure SetParent(NewParent: TWinControl); override;
      procedure Click; override;
      procedure DrawBoxLine; virtual;
    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;

      procedure UpdateLabel; override;

      property LineDrection:Integer read fLineDrection write SetLineDirection;
      property LineLength:Integer read fLineLength write SetLineLength;
      property Line:TShape read fLineShape;
      property LineWidth:Integer read fLineWidth write SetLineWidth;
      property LineMarkColor:TColor read fLineMarkColor write SetLineMarkColor;
      property LineUnMarkColor:TColor read fLineUnMarkColor write SetLineUnMarkColor;
      property AfterLineColorUpdate:TNotifyEvent read fAfterLineColorUpdate write fAfterLineColorUpdate;
    published
  end;

implementation

uses
  LCLType;

const
  _LabelMarginX = 6;
  _LabelMarginY = 7;

{ TLabelBoxLine }

procedure TLabelBoxLine.SetLineDirection(Value: Integer);
begin
  fLineDrection:=Value;
  DrawBoxLine;
end;

procedure TLabelBoxLine.SetLineLength(Value: Integer);
begin
  fLineLength:=Value;
  DrawBoxLine;
end;

procedure TLabelBoxLine.SetLineWidth(Value: Integer);
begin
  fLineWidth:=Value;
  DrawBoxLine;
end;

procedure TLabelBoxLine.SetLineMarkColor(Value: TColor);
begin
  fLineMarkColor:=Value;
  if Toggle then begin
    fLineShape.Pen.Color:=Value;
    fLineShape.Brush.Color:=Value;
  end;
end;

procedure TLabelBoxLine.SetLineUnMarkColor(Value: TColor);
begin
  fLineUnMarkColor:=Value;
  if not Toggle then begin
    fLineShape.Pen.Color:=Value;
    fLineShape.Brush.Color:=Value;
  end;
end;

procedure TLabelBoxLine.UpdateLineColor;
begin
  if Toggle then begin
    fLineShape.Pen.Color:=fColorMark;
    fLineShape.Brush.Color:=fColorMark;
  end
  else begin
    fLineShape.Pen.Color:=fColorUnMark;
    fLineShape.Brush.Color:=fColorUnMark;
  end;
  if assigned(fAfterLineColorUpdate) then
    fAfterLineColorUpdate(self);
end;

procedure TLabelBoxLine.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if Assigned(fLineShape) then
    fLineShape.Parent:=NewParent;
end;

procedure TLabelBoxLine.Click;
begin
  inherited Click;
  UpdateLineColor;
end;

procedure TLabelBoxLine.DrawBoxLine;
var
  PosX, PosY:Integer;
begin
  PosX:=Left+Width div 2;
  PosY:=Top+Height div 2;
  case fLineDrection of
  { down }
  0:  begin
        fLineShape.Width:=fLineWidth;
        fLineShape.Height:=fLineLength;
        fLineShape.Left:=PosX-fLineWidth div 2;
        fLineShape.Top:=PosY;
      end;
  { up }
  1:  begin
        fLineShape.Width:=fLineWidth;
        fLineShape.Height:=fLineLength;
        fLineShape.Left:=PosX-fLineWidth div 2;
        fLineShape.Top:=PosY-fLineLength;
      end;
  { right }
  2:  begin
        fLineShape.Height:=fLineWidth;
        fLineShape.Width:=fLineLength;
        fLineShape.Left:=PosX;
        fLineShape.Top:=PosY-fLineWidth div 2;
      end;
  { left }
  else
      begin
        fLineShape.Height:=fLineWidth;
        fLineShape.Width:=fLineLength;
        fLineShape.Left:=PosX-fLineLength;
        fLineShape.Top:=PosY-fLineWidth div 2;
      end;
  end;
end;

constructor TLabelBoxLine.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fLineLength:=30;
  fLineDrection:=0;
  fLineWidth:=2;
  fLineMarkColor:=clRed;
  fLineUnMarkColor:=clBlue;
  fLineShape:=TShape.Create(self);
  fLineShape.Shape:=stRectangle;
  fLineShape.Pen.Color:=clBlue;
  fLineShape.Brush.Color:=clBlue;
  fLineShape.OnClick:=@OnClickEvent;
  fLineShape.Parent:=Parent;
  DrawBoxLine;
end;

destructor TLabelBoxLine.Destroy;
begin
  inherited Destroy;
end;

procedure TLabelBoxLine.UpdateLabel;
begin
  inherited UpdateLabel;
  DrawBoxLine;
end;

{ TLabelBoxArray }

procedure TLabelBoxArray.SetLabelBound(const Value: TRect);
begin
  fLabelBound:=Value;
end;

procedure TLabelBoxArray.SetLabelHGap(Value: Integer);
begin
  if Value<>fHGap then begin
    fHGap:=Value;
    ReArrangeLabels;
  end;
end;

procedure TLabelBoxArray.SetLabelVGap(Value: Integer);
begin
  if Value<>fVGap then begin
    fVGap:=Value;
    ReArrangeLabels;
  end;
end;

procedure TLabelBoxArray.OnArrayLabelChanged(Sender: TObject);
begin
  ReArrangeLabels;
end;

function TLabelBoxArray.GetMarked(Idx: Integer): Boolean;
var
  i,cidx:Integer;
  temp:TLabelBox;
begin
  i:=0;
  cidx:=-1;
  Result:=False;
  while i<ControlCount do begin
    if Controls[i] is TLabelBox then begin
      temp:=Controls[i] as TLabelBox;
      Inc(cidx);
      if cidx=idx then begin
        Result:=temp.Toggle;
        break;
      end;
    end;
    Inc(i);
  end;
end;

procedure TLabelBoxArray.SetMarked(Idx: Integer; Value: Boolean);
var
  i,cidx:Integer;
  temp:TLabelBox;
begin
  i:=0;
  cidx:=-1;
  while i<ControlCount do begin
    if Controls[i] is TLabelBox then begin
      temp:=Controls[i] as TLabelBox;
      Inc(cidx);
      if cidx=idx then begin
        temp.Toggle:=Value;
        break;
      end;
    end;
    Inc(i);
  end;
end;

function TLabelBoxArray.GetLabelBox(Idx: Integer): TLabelBox;
var
  i,cidx:Integer;
  temp:TLabelBox;
begin
  i:=0;
  cidx:=-1;
  Result:=nil;
  while i<ControlCount do begin
    if Controls[i] is TLabelBox then begin
      temp:=Controls[i] as TLabelBox;
      Inc(cidx);
      if cidx=idx then begin
        Result:=temp;
        break;
      end;
    end;
    Inc(i);
  end;
end;

function TLabelBoxArray.GetLabelBoxName(const LName: TCaption): TLabelBox;
var
  i:Integer;
  temp:TLabelBox;
begin
  i:=0;
  Result:=nil;
  while i<ControlCount do begin
    if Controls[i] is TLabelBox then begin
      temp:=Controls[i] as TLabelBox;
      if temp.Caption=LName then begin
        Result:=temp;
        break;
      end;
    end;
    Inc(i);
  end;
end;

procedure TLabelBoxArray.OnLabelClickEvent(Sender: TObject);
begin
  if assigned(fOnLabelClick) then
    fOnLabelClick(Sender);
end;

procedure TLabelBoxArray.OnLabelDblClickEvent(Sender: TObject);
begin
  if assigned(fOnLabelDblClick) then
    fOnLabelDblClick(Sender);
end;

procedure TLabelBoxArray.BoundsChanged;
begin
  fLabelBound:=BoundsRect;
  inherited BoundsChanged;
end;

constructor TLabelBoxArray.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width:=200;
  Height:=200;
  fHGap:=3;
  fVGap:=3;
  fLabelCount:=0;
  fLabelBound:=BoundsRect;
  fLColorMark:=clRed;
  fLColorUnMark:=clBlue;
  fLabelMarginX:=_LabelMarginX;
  fLabelMarginY:=_LabelMarginY;
end;

destructor TLabelBoxArray.Destroy;
begin
  inherited Destroy;
end;

procedure TLabelBoxArray.AddLabel(const LName: TCaption);
var
  temp:TLabelBox;
begin
  temp:=TLabelBox.Create(self);
  try
    with temp do begin
      Parent:=self;
      MarginX:=fLabelMarginX;
      MarginY:=fLabelMarginY;
      Caption:=LName;
      ColorMark:=fLColorMark;
      ColorUnMark:=fLColorUnMark;
      OnLabelChanged:=@OnArrayLabelChanged;
    end;
    temp.OnClick:=@OnLabelClickEvent;
    temp.OnDblClick:=@OnLabelDblClickEvent;
    ReArrangeLabels;
  except
    temp.Free;
  end;
end;

procedure TLabelBoxArray.DeleteLabel(Idx: Integer);
var
  i,cidx:Integer;
  temp:TLabelBox;
begin
  i:=0;
  cidx:=-1;
  while i<ControlCount do begin
    if Controls[i] is TLabelBox then begin
      temp:=Controls[i] as TLabelBox;
      Inc(cidx);
      if cidx=idx then begin
        temp.Free;
        break;
      end;
    end;
    Inc(i);
  end;
  ReArrangeLabels;
end;

procedure TLabelBoxArray.DeleteLabel(const sLabel: TCaption);
var
  i:Integer;
  temp:TLabelBox;
begin
  i:=ControlCount-1;
  while i>=0 do begin
    if Controls[i] is TLabelBox then begin
      temp:=Controls[i] as TLabelBox;
      if temp.Caption=sLabel then begin
        temp.Free;
        break;
      end;
    end;
    Dec(i);
  end;
  ReArrangeLabels;
end;

procedure TLabelBoxArray.DeleteLabel(Marked: Boolean);
var
  i:Integer;
  temp:TLabelBox;
begin
  i:=ControlCount-1;
  while i>=0 do begin
    if Controls[i] is TLabelBox then begin
      temp:=Controls[i] as TLabelBox;
      if temp.Toggle=Marked then
        temp.Free;
    end;
    Dec(i);
  end;
  ReArrangeLabels;
end;

procedure TLabelBoxArray.InsertLabel(Idx: Integer; const LName: TCaption);
var
  temp:TLabelBox;
begin
  temp:=TLabelBox.Create(self);
  try
    if Idx>=fLabelCount then
      Idx:=fLabelCount-1;
    if Idx<0 then
      Idx:=0;
    InsertControl(temp,Idx);
    with temp do begin
      MarginX:=fLabelMarginX;
      MarginY:=fLabelMarginY;
      Caption:=LName;
      ColorMark:=fLColorMark;
      ColorUnMark:=fLColorUnMark;
      OnLabelChanged:=@OnArrayLabelChanged;
    end;
    temp.OnClick:=@OnLabelClickEvent;
    temp.OnDblClick:=@OnLabelDblClickEvent;
    ReArrangeLabels;
  except
    temp.Free;
  end;
end;

procedure TLabelBoxArray.ReArrangeLabels;
var
  i, PosX, PosY, PosXS, fCount: Integer;
  temp: TLabelBox;
begin
  i:=0;
  PosX:=0;
  PosY:=0;
  fCount:=0;
  while i<ControlCount do begin
    if Controls[i] is TLabelBox then begin
      temp:=Controls[i] as TLabelBox;
      PosXS:=PosX;
      PosX:=PosX+temp.Width+fHGap;
      if PosX>Width then begin
        PosY:=PosY+temp.Height+fVGap;
        PosXS:=0;
        PosX:=PosXS+temp.Width+fHGap;
      end;
      temp.Left:=PosXS;
      temp.Top:=PosY;
      Inc(fCount);
    end;
    Inc(i);
  end;
  fLabelCount:=fCount;
end;

procedure TLabelBoxArray.Clear;
var
  i:Integer;
begin
  i:=ControlCount-1;
  while i>=0 do begin
    if Controls[i] is TLabelBox then
      (Controls[i] as TLabelBox).Free;
    Dec(i);
  end;
  fLabelCount:=0;
end;

{ TLabelBox }

procedure TLabelBox.SetLabelCaption(Value: TCaption);
begin
  fLabel.Caption:=Value;
  UpdateLabel;
end;

function TLabelBox.GetLabelCaption: TCaption;
begin
  Result:=fLabel.Caption;
end;

procedure TLabelBox.OnClickEvent(Sender: TObject);
begin
  Click;
end;

procedure TLabelBox.OnDblClickEvent(Sender: TObject);
begin
  DblClick;
end;

procedure TLabelBox.OnEditorExit(Sender: TObject);
var
  CapText:TCaption;
begin
  CapText:=fEditor.Text;
  if CapText='' then
    CapText:='   ';
  SetLabelCaption(CapText);
  fEditor.Visible:=False;
  if assigned(fOnLabelChanged) then
    fOnLabelChanged(self);
end;

procedure TLabelBox.OnEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift=[] then begin
    if Key=VK_RETURN then
      SetFocus
      else
        if Key=VK_ESCAPE then begin
          fEditor.Text:=fOldCaption;
          SetFocus;
        end;
  end;
end;

procedure TLabelBox.SetColorMark(Value: TColor);
begin
  fColorMark:=Value;
  if fToggle then
    fShape.Pen.Color:=Value;
end;

procedure TLabelBox.SetColorUnMark(Value: TColor);
begin
  fColorUnMark:=Value;
  if not fToggle then
    fShape.Pen.Color:=Value;
end;

procedure TLabelBox.SetLWidthNormal(Value: Integer);
begin
  fLWidthNormal:=Value;
  if not Focused then
    fShape.Pen.Width:=Value;
end;

procedure TLabelBox.SetLWidthFocus(Value: Integer);
begin
  fLWidthFocus:=Value;
  if Focused then
    fShape.Pen.Width:=Value;
end;

procedure TLabelBox.SetToggle(Value: Boolean);
begin
  if fToggle<>Value then begin
    fToggle:=Value;
    if not fToggle then
      fShape.Pen.Color:=fColorUnMark
      else
        fShape.Pen.Color:=fColorMark;
  end;
end;

procedure TLabelBox.SetMarginX(Value: Integer);
begin
  if Value<>fMarginX then begin
    fMarginX:=Value;
    UpdateLabel;
  end;
end;

procedure TLabelBox.SetMarginY(Value: Integer);
begin
  if Value<>fMarginY then begin
    fMarginY:=Value;
    UpdateLabel;
  end;
end;

procedure TLabelBox.SetAutoFit(Value: Boolean);
begin
  if fAutoFit<>Value then begin
    fAutoFit:=Value;
    UpdateLabel;
  end;
end;

procedure TLabelBox.Click;
begin
  if Focused or (not TabStop) then begin
    fToggle:=not fToggle;
    if not fToggle then
      fShape.Pen.Color:=fColorUnMark
      else
        fShape.Pen.Color:=fColorMark;
  end;
  if TabStop then
    SetFocus;
  inherited Click;
end;

procedure TLabelBox.DblClick;
begin
  // enter editor
  fOldCaption:=fLabel.Caption;
  fEditor.Text:=fOldCaption;
  fEditor.Width:=Width-fMarginX*2;
  fEditor.Left:=fMarginX;
  fEditor.Top:=fMarginY;
  fEditor.Visible:=True;
  fEditor.SetFocus;
  inherited DblClick;
end;

procedure TLabelBox.DoEnter;
begin
  fShape.Pen.Width:=fLWidthFocus;
  inherited DoEnter;
end;

procedure TLabelBox.DoExit;
begin
  fShape.Pen.Width:=fLWidthNormal;
  inherited DoExit;
end;

procedure TLabelBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Shift=[] then begin
    // toggle
    if Key=VK_SPACE then
      Click
      else
        // editor
        if (Key=VK_F2) or (Key=VK_RETURN) then
          DblClick;
  end;
  inherited KeyDown(Key, Shift);
end;

constructor TLabelBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // default
  fToggle:=False;
  fAutoFit:=True;
  fColorMark:=clRed;
  fColorUnMark:=clBlue;
  fLWidthNormal:=2;
  fLWidthFocus:=1;
  Width:=30;
  Height:=20;
  TabStop:=True;
  fOldCaption:='';
  fMarginX:=_LabelMarginX;
  fMarginY:=_LabelMarginY;
  // shape
  fShape:=TShape.Create(self);
  fShape.Parent:=self;
  fShape.Shape:=stRectangle;
  fShape.Align:=alClient;
  fShape.Pen.Width:=2;
  fShape.Pen.Color:=fColorUnMark;
  fShape.OnClick:=@OnClickEvent;
  // label
  fLabel:=TLabel.Create(self);
  fLabel.Parent:=self;
  fLabel.AutoSize:=True;
  fLabel.Top:=fMarginY;
  fLabel.Left:=fMarginX;
  fLabel.BringToFront;
  fLabel.Transparent:=True;
  fLabel.ShowAccelChar:=False;
  fLabel.Caption:='Label';
  fLabel.OnClick:=@OnClickEvent;
  fLabel.OnDblClick:=@OnDblClickEvent;
  // editor
  fEditor:=TEdit.Create(self);
  fEditor.Parent:=self;
  fEditor.Visible:=False;
  fEditor.BorderStyle:=bsNone;
  fEditor.OnExit:=@OnEditorExit;
  fEditor.OnKeyDown:=@OnEditorKeyDown;
end;

destructor TLabelBox.Destroy;
begin
  inherited Destroy;
end;

procedure TLabelBox.UpdateLabel;
begin
  fLabel.Left:=fMarginX;
  fLabel.Top:=fMarginY;
  if fAutoFit then begin
    if not fLabel.AutoSize then
      fLabel.AutoSize:=True;
    Width:=fLabel.Width+fMarginX*2;
    Height:=fLabel.Height+fMarginY*2;
  end
  else begin
    if fLabel.AutoSize then
      fLabel.AutoSize:=False;
    fLabel.Width:=Width-fMarginX*2;
    fLabel.Height:=Height-fMarginY*2;
  end;
end;

end.

