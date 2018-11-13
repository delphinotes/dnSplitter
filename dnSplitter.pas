unit dnSplitter;
(*
  dnSplitter - аналог (но не наследник) TSplitter, с кнопкой для скрытия
               связанного контрола

****************************************************************
  Author    : Zverev Nikolay (www.delphinotes.ru)
  Created   : 16.10.2007
  Modified  : 06.11.2018
  Version   : 1.12
****************************************************************

  History:
    ~1.12 06.11.2018
      ! Теперь в момент перемещения сплиттера учитываются Margins и Padding контролов при подсчёте допустимого
        максимального размера
      ! Вернул логику, которая определяет можно ли вызывать WM_SETREDRAW (в UpdateControlSize):
          а) вероятность "провалиться"
             - слишком мала: стабильно воспроизводится только при переключении IsSnapped (например по ShortCut'у
               связанного Action'а) и одноврменном клике мышкой;
             - важность отсутствия мерцания в перерисовке ставлю выше такой вероятности
             - если такое поведение не допустимо - необходимо использовать дополнительный контейнер, например TPanel
          б) глюки прорисовки при dpi > 96 исправляются введением переменной FSkipMoveHandler, см. комментарии по коду

    ~1.11 04.11.2018
      + Добавлена поддержка VCL стилей
      + Добавлена поддержка старых версий Delphi
      * UpdateControlSize вызывает WM_SETREDRAW только если:
        а) Parent не является окном верхнего уровня (иначе есть вероятность "провалиться" сквозь окно,
           а так же наблюдаются глюки прорисовки в Delphi7 при dpi > 96)
        б) включено свойство Parent.DoubleBuffered (при выключенном DoubleBuffered эффект не заметен)
      ! Исправлена проблема поиска AlignControl при первом "кидании" компонента на форму в дизайнере

    ~1.10 16.10.2015
      * Изменён алгоритм определения цвета кнопки при наведении мыши
      * Оптимизация отрисовки в методе DrawArrow
      ! CMDialogKey заменён на CMChildKey (иначе VK_ESCAPE может не дойти до контрола)

    ~1.09 21.09.2015
      * Метод UpdateControlSize обрамлён сообщением WM_SETREDRAW для плавного изменения размеров компонент,
        окружающих сплиттер

    ~1.08 19.11.2012
      + Добавлены свойства ButtonAlign и ButtonPosition
      * Обработка клавиши VK_ESCAPE перенесена в событие CM_DIALOGKEY
      - Убрана ссылка на ActiveControl, которая осталась в наследство от
        стандартного TSplitter, и которая использовалась для подмены обработчика
        OnKeyDown активного элемента управления (эта логика иногда приводила к
        ошибке Stack Overflow)
      ! В методе DrawArrow параметр Offset переименован в AOffset (для
        совместимости с Delphi XE2)

    ~1.07 03.05.2012
      + Реализованы идеи IVK от 02.04.2012:
        а) задержка прорисовки при изменении размера сплиттера, регулируется
           константой RS_UPDATE_DELAY, включается $Define USE_RS_UPDATE_DELAY
        б) если пользователь схватил за кнопку и начал тащить (выйдя за пределы
           2х точек вокруг DownPos), то сплиттер переходит в режим изменения
           размера (а раньше просто срабатывала кнопка)
      + При выключенном AllowDrag курсор для сплиттера по умолчанию сбрасывается
        в crDefault
      ! Исправлена ошибка (не инициализировалась внутренняя переменная FSavedSize)

    ~1.06 12.08.2011
      * Релиз на конкурс http://delphifeeds.ru/ и для читателей блога
        delphinotes.ru (мелкий рефакторинг)

    ~1.05 03.03.2010
      + Перерисовка сплиттера при изменении Enabled (т.к. Enabled может повлиять
         на видимость кнопки)

    ~1.04 07.07.2009
      * Если кнопка отображается, то хинт работает только для кнопки
      + Возможность привязки Splitter'а к Action'у. При этом:
         а) Splitter.IsSnapped = not Action.Checked
         б) Action.AutoCheck автоматически устанавливается в True
         в) Action.DisableIfNoHandler автоматически устанавливается в False
      ! Нормальная обработка VK_ESCAPE (восстанавливается положение сплиттера)
      + При включённом AutoSnap, скрытие происходит не при переходе Size < MinSize
        а при переходе Size <= (MinSize div 2)

    ~1.03 10.04.2008
      * Стабильный релиз
*)

interface

{$i jedi.inc}

{$ifdef DELPHI2010_UP}
  {$define HAS_GESTURES}
  {$define HAS_MARGINS}
  {$define HAS_PADDING}
{$endif}

{$ifdef DELPHIXE2_UP}
  {$define HAS_PARENTDOUBLEBUFFERED}
{$endif}

{$ifdef HAS_UNITSCOPE}
uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Types,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  {$ifdef HAS_UNIT_VCL_THEMES}
  Vcl.Themes,
  {$endif}
  Vcl.ExtCtrls;
{$else}
uses
  Windows,
  Messages,
  SysUtils,
  Types,
  Classes,
  Graphics,
  Controls,
  ExtCtrls;
{$endif}

{.$define USE_RS_UPDATE_DELAY}

const
  MAX_SPLITTER_SIZE = 36;
  {$ifdef USE_RS_UPDATE_DELAY}
  RS_UPDATE_DELAY = 250;
  {$endif}

type
  TdnButtonAlign = (baLeftTop, baCenter, baRightBottom);
  TdnButtonWidthType = (btwPixels, btwPercentage);

  { TdnSplitter }

  TdnSplitter = class;

  {action}
  TdnSplitterActionLink = class(TWinControlActionLink)
  private
    FSaveAutoCheck: Boolean;
    FSaveDisableIfNoHandler: Boolean;
    procedure RestorePrevAction;
  protected
    FClient: TdnSplitter;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetAction(Value: TBasicAction); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  public
    destructor Destroy; override;
  end;
  {/action}

  // TCustomControl, являясь наследником от TWinContorl кушает больше систмных
  // ресурсов (в отличии от TGraphicControl). Однако TCustomControl более чётко
  // реагирует на перемещения мыши, и не вызывает мерцания при изменении размеров
  // родительских контролов

  TdnSplitter = class(TCustomControl)
  private
    FAutoSnap: Boolean;
    FBeveled: Boolean;
    FBrush: TBrush;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldSize: Integer;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSkipMoveHandler: Boolean;
    FSplit: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure AllocateLineDC;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure DrawLine;
    function FindControl: TControl;
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
  private
    FAlignControl: TControl;
    {action}
    FClicksDisabled: Boolean;
    {/action}
    FDownSize: Integer;
    FSavedSize: Integer;
    FSnapped: Boolean;
    FOnSnap: TNotifyEvent;
    FSaveCursor: TCursor;

    FButtonAlign: TdnButtonAlign;
    FButtonPosition: Integer;
    FButtonCursor: TCursor;
    FButtonWidthType: TdnButtonWidthType;
    FButtonWidth: Integer;
    FIsHighlighted: Boolean;
    FButtonVisible: Boolean;
    FButtonColors: array [0..4] of TColor;
    FButtonRect: TRect;

    FAllowDrag: Boolean;  // allow resize with mouse
    FDraging: Boolean;    // now is resizing
    FPainting: Boolean;   // now is painting;
    FSize: Integer;

    {$ifdef USE_RS_UPDATE_DELAY}
    FLastUpdateTime: Cardinal;
    {$endif}

    function IsAlignStored: Boolean;
    function IsCursorStored: Boolean;
    {action}
    function IsSnappedStored: Boolean;
    function IsOnSnapStored: Boolean;
    {/action}
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign);
    function GetAutoHighlightColor: Boolean;
    procedure SetAutoHighlightColor(AValue: Boolean);
    function GetDefaultCursor: TCursor;
    procedure SetAlignControl(AControl: TControl);
    procedure SetAllowDrag(Value: Boolean);
    procedure SetSnapped(const Value: boolean);
    procedure SetSize(ASize: Integer);
    function GetControlSize: Integer;
    procedure SetControlSize(ASize: Integer);
    procedure UpdateWidth;
    procedure UpdatePos;

    procedure SetButtonVisible(const Value: Boolean);
    procedure SetButtonAlign(const Value: TdnButtonAlign);
    procedure SetButtonPosition(const Value: Integer);
    procedure SetButtonWidthType(const Value: TdnButtonWidthType);
    procedure SetButtonWidth(const Value: Integer);

    function GetButtonColor(Index: Integer): TColor;
    procedure SetButtonColor(Index: Integer; const Value: TColor);

    function GetButtonRect: TRect;

    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseEnter(var Msg: TWMMouse); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TWMMouse); message CM_MOUSELEAVE;
    //procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoSnap; dynamic;
    procedure CheckHighlighted(MousePos: TPoint); overload;
    procedure CheckHighlighted(Highlighted: Boolean); overload;

    procedure PaintButton;
    function DrawArrow(ACanvas: TCanvas; ARect: TRect; AOffset: Integer; ArrowSize: Integer; AColor: TColor): Integer;
  protected
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RequestAlign; override;
    procedure Paint; override;
    procedure StopSizing; dynamic;
    {action}
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure Click; override;
    {/action}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelDrag;
    {action}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    {/action}
    property ButtonRect: TRect read FButtonRect;
    property Canvas;
    property Draging: Boolean read FDraging;
  published
    {action}
    property Action;
    {/action}
    property Align read GetAlign write SetAlign stored IsAlignStored;
    property AlignControl: TControl read FAlignControl write SetAlignControl;
    property AllowDrag: Boolean read FAllowDrag write SetAllowDrag default True;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property Beveled: Boolean read FBeveled write SetBeveled default False;
    property Color;
    property Cursor stored IsCursorStored default crHSplit;
    property Constraints;
    property ControlSize: Integer read GetControlSize write SetControlSize stored FSnapped;
    {$ifdef HAS_PARENTDOUBLEBUFFERED}
    property DoubleBuffered;
    {$endif}
    property Enabled;
    property IsSnapped: Boolean read FSnapped write SetSnapped stored IsSnappedStored default False;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property ParentBackground default True;
    {$ifdef HAS_PARENTDOUBLEBUFFERED}
    property ParentDoubleBuffered;
    {$endif}
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsUpdate;
    property ShowHint;
    property Size: Integer read FSize write SetSize default 8;
    property Visible;
    property Width stored False;
    property Height stored False;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnSnap: TNotifyEvent read FOnSnap write FOnSnap stored IsOnSnapStored;
    // Button:
    property ButtonCursor: TCursor read FButtonCursor write FButtonCursor default crHandPoint;
    property ButtonVisible: Boolean read FButtonVisible write SetButtonVisible default True;
    property ButtonAlign: TdnButtonAlign read FButtonAlign write SetButtonAlign default baCenter;
    property ButtonPosition: Integer read FButtonPosition write SetButtonPosition default 0;
    property ButtonWidthType: TdnButtonWidthType read FButtonWidthType write SetButtonWidthType default btwPixels;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 100;
    // Colors:
    property AutoHighlightColor: Boolean read GetAutoHighlightColor write SetAutoHighlightColor stored False;
    property ArrowColor: TColor index 0 read GetButtonColor write SetButtonColor default clHighlight;
    property ButtonColor: TColor index 1 read GetButtonColor write SetButtonColor default clBtnFace;
    property ButtonHighlightColor: TColor index 2 read GetButtonColor write SetButtonColor default clDefault;
    property TextureColor1: TColor index 3 read GetButtonColor write SetButtonColor default clBtnHighlight;
    property TextureColor2: TColor index 4 read GetButtonColor write SetButtonColor default clBtnShadow;
  end;

procedure Register;

implementation

{$ifdef HAS_UNITSCOPE}
uses
  System.Math,
  Vcl.ActnList,
  Vcl.Forms;
{$else}
uses
  Math,
  ActnList,
  Forms;
{$endif}

procedure Register;
begin
  RegisterComponents('Delphi Notes.RU', [TdnSplitter]);
end;

{ Misc }

type
  TArrowDirection = (adLeft, adRight, adUp, adDown);

function AlignToDirection(AAlign: TAlign; AInvert: Boolean): TArrowDirection; {$ifdef SUPPORTS_INLINE}inline;{$endif}
begin
  case AAlign of
    alLeft:
      if AInvert then
        Result := adRight
      else
        Result := adLeft;
    alRight:
      if AInvert then
        Result := adLeft
      else
        Result := adRight;
    alTop:
      if AInvert then
        Result := adDown
      else
        Result := adUp;
  else //alBottom and other
    if AInvert then
      Result := adUp
    else
      Result := adDown;
  end;
end;

procedure DoDrawArrow(ACanvas: TCanvas; AColor: TColor; const ARect: TRect; ADirection: TArrowDirection);
  function CenterX(const ARect: TRect): Integer; {$ifdef SUPPORTS_INLINE}inline;{$endif}
  begin
    Result := ARect.Left + (ARect.Right - ARect.Left) div 2;
  end;

  function CenterY(const ARect: TRect): Integer; {$ifdef SUPPORTS_INLINE}inline;{$endif}
  begin
    Result := ARect.Top + (ARect.Bottom - ARect.Top) div 2;
  end;
begin
  if AColor = clNone then
    Exit;
  ACanvas.Pen.Color := AColor;
  ACanvas.Brush.Color := AColor;
  case ADirection of
    adLeft:
      ACanvas.Polygon([Point(ARect.Right, ARect.Top), Point(ARect.Right, ARect.Bottom),
        Point(ARect.Left, CenterY(ARect))]);
    adRight:
      ACanvas.Polygon([Point(ARect.Left, ARect.Top), Point(ARect.Left, ARect.Bottom),
        Point(ARect.Right, CenterY(ARect))]);
    adUp:
      ACanvas.Polygon([Point(ARect.Left, ARect.Bottom), Point(ARect.Right, ARect.Bottom),
        Point(CenterX(ARect), ARect.Top)]);
    adDown:
      ACanvas.Polygon([Point(ARect.Left, ARect.Top), Point(ARect.Right, ARect.Top),
        Point(CenterX(ARect), ARect.Bottom)]);
  else
    Assert(False);
  end;
end;

function CalcButtonHighlightColor: TColor;
  function CalcValue(C1, C2: Byte): Byte; {$ifdef SUPPORTS_INLINE}inline;{$endif}
  begin
    Result := (C1 * 20 + C2 * 80 + 50) div 100;
  end;
var
  C1, C2: TColor;
begin
  C1 := GetSysColor(clHighlight and $FF);
  C2 := GetSysColor(clHighlightText and $FF);

  Result := RGB(
    CalcValue(GetRValue(C1), GetRValue(C2)),
    CalcValue(GetGValue(C1), GetGValue(C2)),
    CalcValue(GetBValue(C1), GetBValue(C2))
  );
end;

{ TswSplitterActionLink }

destructor TdnSplitterActionLink.Destroy;
begin
  RestorePrevAction;
  inherited Destroy;
end;

procedure TdnSplitterActionLink.RestorePrevAction;
begin
  if Action is TCustomAction then
    with TCustomAction(Action) do
    begin
      AutoCheck := FSaveAutoCheck;
      DisableIfNoHandler := FSaveDisableIfNoHandler;
    end;
end;

procedure TdnSplitterActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TdnSplitter;
end;

function TdnSplitterActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.IsSnapped = not (Action as TCustomAction).Checked);
end;

function TdnSplitterActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := @FClient.OnSnap = @Action.OnExecute;
end;

procedure TdnSplitterActionLink.SetAction(Value: TBasicAction);
begin
  RestorePrevAction;

  inherited SetAction(Value);

  if Action is TCustomAction then
    with TCustomAction(Action) do
    begin
      FSaveAutoCheck := AutoCheck;
      FSaveDisableIfNoHandler := DisableIfNoHandler;
      AutoCheck := True;
      DisableIfNoHandler := False;
    end;
end;

procedure TdnSplitterActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
  begin
    FClient.FClicksDisabled := True;
    try
      FClient.CancelDrag;
      FClient.IsSnapped := not Value;
    finally
      FClient.FClicksDisabled := False;
    end;
  end;
end;

procedure TdnSplitterActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then
    FClient.OnSnap := Value;
end;

{ TdnSplitter }

constructor TdnSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque, csParentBackground]
    {$ifdef HAS_GESTURES} - [csGestures]{$endif}
  ;
  FAutoSnap := True;

  FSize := 8;

  Cursor := crHSplit;
  FMinSize := 30;
  FResizeStyle := rsUpdate;
  FOldSize := -1;

  FButtonCursor := crHandPoint;
  FSavedSize := -1;
  FButtonWidth := 100;
  FButtonVisible := True;
  FButtonColors[0] := clHighlight;
  FButtonColors[1] := clBtnFace;
  FButtonColors[2] := clDefault;
  FButtonColors[3] := clBtnHighlight;
  FButtonColors[4] := clBtnShadow;

  FAllowDrag := True;
  FButtonAlign := baCenter;
  FButtonWidthType := btwPixels;
end;

destructor TdnSplitter.Destroy;
begin
  FreeAndNil(FBrush);
  inherited Destroy;
end;

procedure TdnSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
      {$ifdef HAS_UNIT_VCL_THEMES}
      if TStyleManager.IsCustomStyleActive then
        with StyleServices do
          FBrush.Bitmap := AllocPatternBitmap(clBlack, GetStyleColor(scSplitter))
      else
      {$endif}
        FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TdnSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if Align in [alLeft, alRight] then
    P.X := Left + FSplit
  else
    P.Y := Top + FSplit;
  with P do
    PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TdnSplitter.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  FreeAndNil(FBrush);
end;

function TdnSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  {$ifdef HAS_MARGINS}
  if AlignWithMargins then
  case Align of
    alLeft:
      Dec(P.X, Margins.Left + 1);
    alRight:
      Inc(P.X, Width + Margins.Right + 1);
    alTop:
      Dec(P.Y, Margins.Top + 1);
    alBottom:
      Inc(P.Y, Height + Margins.Bottom + 1);
  else
    Exit;
  end else
  {$endif}
  case Align of
    alLeft:
      Dec(P.X);
    alRight:
      Inc(P.X, Width);
    alTop:
      Dec(P.Y);
    alBottom:
      Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled and
      (Result.Align in [alLeft, alRight, alTop, alBottom]) and
     ((Result.Align in [alLeft, alRight]) = (Align in [alLeft, alRight])) then
    begin
      R := Result.BoundsRect;
      {$ifdef HAS_MARGINS}
      if Result.AlignWithMargins then
      begin
        Inc(R.Right, Result.Margins.Right);
        Dec(R.Left, Result.Margins.Left);
        Inc(R.Bottom, Result.Margins.Bottom);
        Dec(R.Top, Result.Margins.Top);
      end;
      {$endif}
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then Exit;
    end;
  end;
  Result := nil;
end;

procedure TdnSplitter.RequestAlign;
begin
  inherited RequestAlign;

  if AllowDrag and (Cursor <> crVSplit) and (Cursor <> crHSplit) then
    Exit;
  if not AllowDrag and (Cursor <> crDefault) then
    Exit;
  
  Cursor := GetDefaultCursor;
end;

procedure TdnSplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  R: TRect;
begin
  FPainting := True;

  R := ClientRect;

  {$ifdef HAS_UNIT_VCL_THEMES}
  if not StyleServices.Enabled or not ParentBackground or not (seClient in StyleElements) then
  begin
    if TStyleManager.IsCustomStyleActive then
      Canvas.Brush.Color := StyleServices.GetSystemColor(clBtnFace)
    else
  {$endif}
      Canvas.Brush.Color := Color;

    Canvas.FillRect(ClientRect);
  {$ifdef HAS_UNIT_VCL_THEMES}
  end;
  {$endif}

  if Beveled then
  begin
    if Align in [alLeft, alRight] then
      InflateRect(R, 0, 1)
    else
      InflateRect(R, 1, 0);

    {$ifdef HAS_UNIT_VCL_THEMES}
    if TStyleManager.IsCustomStyleActive then
      Canvas.Brush.Color := StyleServices.GetSystemColor(clBtnShadow)
    else
    {$endif}
      Canvas.Brush.Color := clBtnShadow;
    Canvas.FrameRect(R);
  end;

  if (csDesigning in ComponentState) and (not Beveled) then
    // Draw outline:
    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Color := XorColor;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;

  PaintButton;

  if Assigned(FOnPaint) then
    FOnPaint(Self);

  FPainting := False;
end;

function TdnSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then
    FOnCanResize(Self, NewSize, Result);
end;

procedure TdnSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
  C: TControl;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if not Enabled then
    Exit;

  if not Assigned(FAlignControl) then
    Exit;

  if Button <> mbLeft then
    Exit;

  if IsSnapped then
    FDownSize := 0 else
  case Align of
    alLeft, alRight:
      FDownSize := FAlignControl.Width;
    alTop, alBottom:
      FDownSize := FAlignControl.Height;
  end;

  if AllowDrag then
  // default handler:
  begin
    FDownPos := Point(X, Y);

    FDraging := True;
    if Align in [alLeft, alRight] then
    begin
      FMaxSize := Parent.ClientWidth
        {$ifdef HAS_PADDING}
        - Parent.Padding.Left
        - Parent.Padding.Right
        {$endif}
        - FMinSize
      ;
      for I := 0 to Parent.ControlCount - 1 do
      begin
        C := Parent.Controls[I];
        if (C <> FAlignControl) and C.Visible and (C.Align in [alLeft, alRight]) then
          {$ifdef HAS_MARGINS}
          if C.AlignWithMargins then
            Dec(FMaxSize, C.Width + C.Margins.Left + C.Margins.Right)
          else
          {$endif}
            Dec(FMaxSize, C.Width);
      end;
    end else
    begin
      FMaxSize := Parent.ClientHeight
        {$ifdef HAS_PADDING}
        - Parent.Padding.Top
        - Parent.Padding.Bottom
        {$endif}
        - FMinSize
      ;
      for I := 0 to Parent.ControlCount - 1 do
      begin
        C := Parent.Controls[I];
        if (C <> FAlignControl) and C.Visible and (C.Align in [alTop, alBottom]) then
          {$ifdef HAS_MARGINS}
          if C.AlignWithMargins then
            Dec(FMaxSize, C.Height + C.Margins.Top + C.Margins.Bottom)
          else
          {$endif}
            Dec(FMaxSize, C.Height);
      end;
    end;
    UpdateSize(X, Y);
    AllocateLineDC;
    if ResizeStyle in [rsLine, rsPattern] then
      DrawLine;
  end else
  if FIsHighlighted then
    IsSnapped := not IsSnapped;
end;

procedure TdnSplitter.UpdateControlSize;
var
  LLockPaint: Boolean;
begin
  if FNewSize <> FOldSize then
  begin
    //LLockPaint := Parent.DoubleBuffered and Parent.Visible and Assigned(Parent.Parent) and Parent.HandleAllocated;
    LLockPaint := Parent.Visible and Parent.HandleAllocated;
    if LLockPaint then
      SendMessage(Parent.Handle, WM_SETREDRAW, 0, 0);
    try
      case Align of
        alLeft:
          FAlignControl.Width := FNewSize;
        alTop:
          FAlignControl.Height := FNewSize;
        alRight:
          begin
            Parent.DisableAlign;
            try
              FAlignControl.Left := FAlignControl.Left + (FAlignControl.Width - FNewSize);
              FAlignControl.Width := FNewSize;
            finally
              Parent.EnableAlign;
            end;
          end;
        alBottom:
          begin
            Parent.DisableAlign;
            try
              FAlignControl.Top := FAlignControl.Top + (FAlignControl.Height - FNewSize);
              FAlignControl.Height := FNewSize;
            finally
              Parent.EnableAlign;
            end;
          end;
      end;

      if Assigned(FOnMoved) then
        FOnMoved(Self);

      FOldSize := FNewSize;
    finally
      if LLockPaint then
      begin
        SendMessage(Parent.Handle, WM_SETREDRAW, 1, 0);
        RedrawWindow(Parent.Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_ALLCHILDREN or RDW_UPDATENOW or RDW_FRAME);
        FSkipMoveHandler := True;
      end else
      if Parent.HandleAllocated then
        Parent.Repaint;
    end;
  end;
end;

procedure TdnSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;

  S := 0;
  case Align of
    alLeft:
      S := FAlignControl.Width + Split;
    alRight:
      S := FAlignControl.Width - Split;
    alTop:
      S := FAlignControl.Height + Split;
    alBottom:
      S := FAlignControl.Height - Split;
  end;
  NewSize := S;

  if FAutoSnap and (S <= (MinSize div 2)) then
    NewSize := 0
  else if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;

  if (S <> NewSize) and (ResizeStyle in [rsLine, rsPattern]) then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize
    else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

procedure TdnSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TdnSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
  {$ifdef USE_RS_UPDATE_DELAY}
  Tick: Cardinal;
  {$endif}
begin
  inherited;

  if FSkipMoveHandler then
  begin
    // Если приложение dpiAware и включено масштабирование в Windows (DPI > 96),
    // то после применения WM_SETREDRAW (в обработчике UpdateControlSize)
    // к форме (случай, когда Parent is TCustomForm)
    // в WM_MOUSEMOVE почему-то прилетают кривые координаты, но только единожды.
    // Пропускаем логику один раз, затем возобновляем
    // HINT: здесь нет дополнительных проверок на описанный случай, т.к. один раз пропустить
    // обработчик после изменения размеров AlignControl'а не страшно: при любых обстоятельствах
    // WM_MOUSEMOVE вызывается достаточно часто
    FSkipMoveHandler := False;
    Exit;
  end;

  if FDraging then
  begin
    if FIsHighlighted then
    begin
      if PtInRect(Rect(X - 2, Y - 2, X + 2, Y + 2), FDownPos) then
        Exit;
      CheckHighlighted(False)
    end;

    CalcSplitSize(X, Y, NewSize, Split);
    if CanResize(NewSize) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then
        DrawLine;

      FNewSize := NewSize;
      FSplit := Split;

      if ResizeStyle in [rsLine, rsPattern] then
        DrawLine else
      if ResizeStyle = rsUpdate then
      begin
        {$ifdef USE_RS_UPDATE_DELAY}
        Tick := GetTickCount;
        if (Tick - FLastUpdateTime > RS_UPDATE_DELAY) or (Tick < FLastUpdateTime) then
        begin
          FLastUpdateTime := Tick;
          UpdateControlSize;
        end;
        {$else}
        UpdateControlSize;
        {$endif}
        IsSnapped := FNewSize = 0;
      end;
    end;
  end else
  if not (csDesigning in ComponentState) then
  begin
    CheckHighlighted(Point(X, Y));
  end;
end;

procedure TdnSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Button <> mbLeft then
    Exit;

  if FDraging then
  begin
    if FIsHighlighted then
    begin
      FDraging := False;
      IsSnapped := not IsSnapped;
    end else
    begin
      if ResizeStyle in [rsLine, rsPattern] then
        DrawLine;

      UpdateControlSize;
      IsSnapped := FNewSize = 0;

      StopSizing;

      FDraging := False;
    end
  end;

  if not (csDesigning in ComponentState) then
    CheckHighlighted(Point(X, Y));
end;

procedure TdnSplitter.CancelDrag;
begin
  if FDraging then
  begin
    // "отпускаем" мышь:
    ReleaseCapture;

    if ResizeStyle in [rsLine, rsPattern] then
      DrawLine;

    // возвращаем размер, который был до захвата мыши:
    FNewSize := FDownSize;
    UpdateControlSize;
    IsSnapped := FNewSize = 0;

    StopSizing;

    FDraging := False;
  end;
end;

procedure TdnSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TdnSplitter.StopSizing;
begin
  if Assigned(FAlignControl) then
  begin
    if FLineVisible then
      DrawLine;
    ReleaseLineDC;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

procedure TdnSplitter.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.IsSnapped = False) then
        Self.IsSnapped := not Checked;
      if not CheckDefaults or not Assigned(Self.OnSnap) then
        Self.OnSnap := OnExecute;
    end;
end;

function TdnSplitter.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TdnSplitterActionLink;
end;

procedure TdnSplitter.Click;
begin
  // skip inherited
end;

function TdnSplitter.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action);

  // это нужно для того, чтобы action отрабатывал даже в том случае,
  // если у него нету своего обработчика
  if Action = Self.Action then
    Result := True;
end;

procedure TdnSplitter.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if Assigned(AParent) and (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    // обрабатываем момент, когда компонент впервые "кидается" на форму в дизайнере
    inherited Align := alLeft;
    AlignControl := FindControl;
    UpdateWidth;
  end;
end;

procedure TdnSplitter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAlignControl) then
  begin
    FAlignControl := nil;
    FSnapped := False;
    Align := alNone;
  end;
end;

function TdnSplitter.IsAlignStored: Boolean;
begin
  Result := (FAlignControl = nil);
end;

function TdnSplitter.IsCursorStored: Boolean;
begin
  Result := Cursor <> GetDefaultCursor;
end;

function TdnSplitter.IsSnappedStored: Boolean;
begin
  Result := (ActionLink = nil) or not TdnSplitterActionLink(ActionLink).IsCheckedLinked;
end;

function TdnSplitter.IsOnSnapStored: Boolean;
begin
  Result := (ActionLink = nil) or not TdnSplitterActionLink(ActionLink).IsOnExecuteLinked;
end;

function TdnSplitter.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure TdnSplitter.SetAlign(Value: TAlign);
begin
  if csLoading in ComponentState then
  begin
    inherited Align := Value;
    UpdateWidth;
    Exit;
  end;

  if Value <> alClient then
  begin
    AlignControl := nil;

    inherited Align := Value;

    if Value in [alTop, alBottom, alLeft, alRight] then
      AlignControl := FindControl;
  end;
end;

function TdnSplitter.GetAutoHighlightColor: Boolean;
begin
  Result := ButtonHighlightColor = clDefault;
end;

procedure TdnSplitter.SetAutoHighlightColor(AValue: Boolean);
begin
  if AValue then
    ButtonHighlightColor := clDefault;
end;

function TdnSplitter.GetDefaultCursor: TCursor;
begin
  if not AllowDrag then
    Result := crDefault
  else if Align in [alBottom, alTop] then
    Result := crVSplit
  else
    Result := crHSplit;
end;

procedure TdnSplitter.SetAlignControl(AControl: TControl);
begin
  // не дадим назначить себя
  if AControl = Self then
    Exit;

  if FAlignControl <> AControl then
  begin
    // если у назначаемого контрола не выставлено Align к какому-нить боку
    if Assigned(AControl) and not (AControl.Align in [alTop, alBottom, alLeft, alRight]) then
      Exit;

    if Assigned(FAlignControl) then
      IsSnapped := False;

    // если новый контрол не определён, попробуем найти автоматически:
    if not Assigned(AControl)
      then FAlignControl := FindControl
      else FAlignControl := AControl;

    // приконнектимся к найденному контролу
    if Assigned(FAlignControl) then
    begin
      inherited Align := alNone;

      UpdatePos;

      inherited Align := FAlignControl.Align;

      UpdateWidth;

      GetControlSize;
    end;
  end;
end;

procedure TdnSplitter.SetAllowDrag(Value: Boolean);
var
  IsDefaultCursor: Boolean;
begin
  if FAllowDrag <> Value then
  begin
    IsDefaultCursor := Cursor = GetDefaultCursor;
    FAllowDrag := Value;
    if IsDefaultCursor then
      Cursor := GetDefaultCursor;
  end;
end;

procedure TdnSplitter.SetSnapped(const Value: boolean);
begin
  if csLoading in ComponentState then
  begin
    FSnapped := Value;
    Exit;
  end;

  if FAlignControl = nil then
  begin
    FSnapped := False;
    Exit;
  end;

  if FSnapped <> Value then
  begin
    if not FDraging then
    begin
      if Value then
      begin
        begin
          case Align of
            alLeft, alRight:
              FSavedSize := FAlignControl.Width;
            alTop, alBottom:
              FSavedSize := FAlignControl.Height;
          else
            begin
              FSnapped := False;
              Exit;
            end;
          end;
        end;
          FNewSize := 0;
      end else
        FNewSize := FSavedSize;

      UpdateControlSize;
    end;

    FSnapped := Value;
    {action}
    if not FClicksDisabled then
    {/action}
      DoSnap;

    {action}
    if (Action is TCustomAction) then
      TCustomAction(Action).Checked := not FSnapped;
    {/action}

    UpdatePos;
    Invalidate;
  end;
end;

procedure TdnSplitter.DoSnap;
begin
  // Если есть Action и его метод OnExecute не совпадает с методом FOnSnap,
  // то вызывается FOnSnap:
  if Assigned(FOnSnap) and (Action <> nil) and (@FOnSnap <> @Action.OnExecute) then
    FOnSnap(Self) else
  // Если есть Action - вызываем его OnExecute
  if not (csDesigning in ComponentState) and (ActionLink <> nil) then
    ActionLink.Execute(Self) else
  // В остальных случаях:
  if Assigned(FOnSnap) then
    FOnSnap(Self);
end;

procedure TdnSplitter.CheckHighlighted(MousePos: TPoint);
begin
  CheckHighlighted(FButtonVisible and PtInRect(GetButtonRect, MousePos));
end;

procedure TdnSplitter.CheckHighlighted(Highlighted: Boolean);
var
  R: TRect;
begin
  if FIsHighlighted <> Highlighted then
  begin
    FIsHighlighted := Highlighted;

    if FIsHighlighted then
    begin
      FSaveCursor := Cursor;
      Cursor := FButtonCursor;
    end else
    begin
      // сбрасываем Hint, т.к. если кнопка включена, то Hint работает только для неё
      Application.CancelHint;
      Cursor := FSaveCursor;
      if GetCapture <> 0 then
        // Force Refresh Cursor
        Perform(WM_SETCURSOR, Handle, HTCLIENT);
    end;

    //PaintButton;
    R := GetButtonRect;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TdnSplitter.SetSize(ASize: Integer);
begin
  if (FSize <> ASize) and (ASize > 0) and (ASize <= MAX_SPLITTER_SIZE) then
  begin
    FSize := ASize;
    UpdateWidth;
  end;
end;

function TdnSplitter.GetControlSize: Integer;
begin
  if not IsSnapped and Assigned(FAlignControl) then
    case Align of
      alLeft, alRight:
        FSavedSize := FAlignControl.Width;
      alTop, alBottom:
        FSavedSize := FAlignControl.Height;
    end;

  Result := FSavedSize;
end;

procedure TdnSplitter.SetControlSize(ASize: Integer);
begin
  FSavedSize := ASize;
  if not IsSnapped and Assigned(FAlignControl) then
    case Align of
      alLeft, alRight:
        FAlignControl.Width := FSavedSize;
      alTop, alBottom:
        FAlignControl.Height := FSavedSize;
    end;
end;

procedure TdnSplitter.UpdateWidth;
begin
  if Align in [alLeft, alRight] then
    Width := FSize else
  if Align in [alTop, alBottom] then
    Height := FSize;
end;

procedure TdnSplitter.UpdatePos;
begin
  if Assigned(FAlignControl) then
    case FAlignControl.Align of
      alTop:
        Top := FAlignControl.Top + FAlignControl.Height;
      alBottom:
        Top := FAlignControl.Top - Width;
      alLeft:
        Left := FAlignControl.Left + FAlignControl.Width;
      alRight:
        Left := FAlignControl.Left - Width;
    end;
end;

procedure TdnSplitter.PaintButton;
const
  TEXTURE_SIZE = 3;
var
  BtnRect: TRect;
  BW: Integer;
  TextureBmp: TBitmap;
  x, y: Integer;
  RW, RH: Integer;
  OffscreenBmp: TBitmap;
  LBkColor: TColor;
  LFrameColor1: TColor;
  LFrameColor2: TColor;
  LArrowColor: TColor;
  LDotColor1: TColor;
  LDotColor2: TColor;
begin
  if (not FButtonVisible) or (not Enabled) then
    Exit;

  BtnRect := GetButtonRect;
  if IsRectEmpty(BtnRect) then
    Exit; // nothing to draw

  // Get Colors:
  {$ifdef HAS_UNIT_VCL_THEMES}
  if TStyleManager.IsCustomStyleActive then
  begin
    LFrameColor1 := StyleServices.GetSystemColor(clBtnShadow);
    LFrameColor2 := StyleServices.GetSystemColor(clBtnHighlight);

    if FIsHighlighted then
    begin
      LBkColor := StyleServices.GetSystemColor(clHighlight);
      LArrowColor := StyleServices.GetSystemColor(clBtnFace);
      LDotColor1 := StyleServices.GetSystemColor(clHighlight);
      LDotColor2 := StyleServices.GetSystemColor(clBtnFace);
    end else
    begin
      LBkColor := StyleServices.GetSystemColor(clBtnFace);
      LArrowColor := StyleServices.GetSystemColor(clBtnShadow);
      LDotColor1 := StyleServices.GetSystemColor(clBtnHighlight);
      LDotColor2 := StyleServices.GetSystemColor(clBtnShadow);
    end;
  end else
  {$endif}
  begin
    LFrameColor1 := clBtnShadow;
    LFrameColor2 := clWindow;

    if FIsHighlighted then
    begin
      LBkColor := ButtonHighlightColor;
      if LBkColor = clDefault then
        LBkColor := CalcButtonHighlightColor;
    end else
      LBkColor := ButtonColor;

    LArrowColor := ArrowColor;
    LDotColor1 := TextureColor1;
    LDotColor2 := TextureColor2;
  end;

  OffscreenBmp := TBitmap.Create;
  try
    OffsetRect(BtnRect, -BtnRect.Left, -BtnRect.Top);
    OffscreenBmp.Width := BtnRect.Right;
    OffscreenBmp.Height := BtnRect.Bottom;

    begin
      // Draw basic button
      OffscreenBmp.Canvas.Brush.Color := LFrameColor1;
      OffscreenBmp.Canvas.FrameRect(BtnRect);
      InflateRect(BtnRect, -1, -1);

      OffscreenBmp.Canvas.Pen.Color := LFrameColor2;
      with BtnRect, OffscreenBmp.Canvas do
      begin
        MoveTo(Left, Bottom - 1);
        LineTo(Left, Top);
        LineTo(Right, Top);
      end;
      Inc(BtnRect.Left);
      Inc(BtnRect.Top);

      OffscreenBmp.Canvas.Brush.Color := LBkColor;
      OffscreenBmp.Canvas.FillRect(BtnRect);

      Dec(BtnRect.Right);
      Dec(BtnRect.Bottom);

      // Draw the insides of the button
      with BtnRect do
      begin
        // Draw the arrows
        if Align in [alLeft, alRight] then
        begin
          InflateRect(BtnRect, 0, -4);
          BW := BtnRect.Right - BtnRect.Left;
          DrawArrow(OffscreenBmp.Canvas, BtnRect, 1, BW, LArrowColor);
          BW := DrawArrow(OffscreenBmp.Canvas, BtnRect, -1, BW, LArrowColor);
          InflateRect(BtnRect, 0, -(BW + 4));
        end else begin
          InflateRect(BtnRect, -4, 0);
          BW := BtnRect.Bottom - BtnRect.Top;
          DrawArrow(OffscreenBmp.Canvas, BtnRect, 1, BW, LArrowColor);
          BW := DrawArrow(OffscreenBmp.Canvas, BtnRect, -1, BW, LArrowColor);
          InflateRect(BtnRect, -(BW + 4), 0);
        end;

        // Draw the texture
        // Note: This is so complex because I'm trying to make as much like the
        //       Netscape splitter as possible.  They use a 3x3 texture pattern, and
        //       that's harder to tile.  If the had used an 8x8 (or smaller
        //       divisibly, i.e. 2x2 or 4x4), I could have used Brush.Bitmap and
        //       FillRect and they whole thing would have been about half the size,
        //       twice as fast, and 1/10th as complex.
        RW := BtnRect.Right - BtnRect.Left;
        RH := BtnRect.Bottom - BtnRect.Top;
        if (RW >= TEXTURE_SIZE) and (RH >= TEXTURE_SIZE) then
        begin
          TextureBmp := TBitmap.Create;
          try
            with TextureBmp do
            begin
              Width := RW;
              Height := RH;
              // Draw first square
              Canvas.Brush.Color := LBkColor;
              Canvas.FillRect(Rect(0, 0, RW+1, RH+1));
              if LDotColor1 <> clNone then
                Canvas.Pixels[1,1] := LDotColor1;
              if LDotColor2 <> clNone then
                Canvas.Pixels[2,2] := LDotColor2;

              // Tile first square all the way across
              for x := 1 to ((RW div TEXTURE_SIZE) + ord(RW mod TEXTURE_SIZE > 0)) do
              begin
                Canvas.CopyRect(Bounds(x * TEXTURE_SIZE, 0, TEXTURE_SIZE,
                   TEXTURE_SIZE), Canvas, Rect(0, 0, TEXTURE_SIZE, TEXTURE_SIZE));
              end;

              // Tile first row all the way down
              for y := 1 to ((RH div TEXTURE_SIZE) + ord(RH mod TEXTURE_SIZE > 0)) do
              begin
                Canvas.CopyRect(Bounds(0, y * TEXTURE_SIZE, RW, TEXTURE_SIZE),
                   Canvas, Rect(0, 0, RW, TEXTURE_SIZE));
              end;

              // Above could be better if it reversed process when splitter was
              // taller than it was wider.  Optimized only for horizontal right now.
            end;
            // Copy texture bitmap to the screen.
            OffscreenBmp.Canvas.CopyRect(BtnRect, TextureBmp.Canvas,
               Rect(0, 0, RW, RH));
          finally
            TextureBmp.Free;
          end;
        end;
      end;
    end;

    Canvas.CopyRect(ButtonRect, OffscreenBmp.Canvas, Rect(0, 0, OffscreenBmp.Width, OffscreenBmp.Height));
  finally
    OffscreenBmp.Free;
  end;
end;

function TdnSplitter.DrawArrow(ACanvas: TCanvas; ARect: TRect; AOffset: Integer;
   ArrowSize: Integer; AColor: TColor): Integer;
var
  LArrowDirection: TArrowDirection;
  LArrowRect: TRect;

  procedure DoCalcArrowRect;
  begin
    case LArrowDirection of
      adLeft, adRight:
        begin
          LArrowRect.Left := ARect.Left + (ARect.Right - ARect.Left - ArrowSize) div 2 + 1;
          LArrowRect.Right := LArrowRect.Left + ArrowSize - 1;

          if AOffset < 0 then
            LArrowRect.Top := ARect.Bottom + AOffset - Result
          else
            LArrowRect.Top := ARect.Top + AOffset;
          LArrowRect.Bottom := LArrowRect.Top + Result - 1;
        end;
    else
      if AOffset < 0 then
        LArrowRect.Left := ARect.Right + AOffset - Result
      else
        LArrowRect.Left := ARect.Left + AOffset;
      LArrowRect.Right := LArrowRect.Left + Result - 1;

      LArrowRect.Top := ARect.Top + (ARect.Bottom - ARect.Top - ArrowSize) div 2 + 1;
      LArrowRect.Bottom := LArrowRect.Top + ArrowSize - 1;
    end;
  end;

begin
  if not Odd(ArrowSize) then
    Dec(ArrowSize);
  ArrowSize := Min(Max(1, ArrowSize), ButtonWidth div 5);
  Result := ArrowSize * 2 - 1;

  LArrowDirection := AlignToDirection(Align, FSnapped);
  DoCalcArrowRect;
  DoDrawArrow(ACanvas, AColor, LArrowRect, LArrowDirection);
end;

procedure TdnSplitter.SetButtonVisible(const Value: boolean);
begin
  if FButtonVisible <> Value then
  begin
    FButtonVisible := Value;
    SetRectEmpty(FButtonRect);
    Invalidate;
  end;
end;

procedure TdnSplitter.SetButtonAlign(const Value: TdnButtonAlign);
begin
  if FButtonAlign <> Value then
  begin
    FButtonAlign := Value;
    if FButtonVisible then
      Invalidate;
  end;
end;

procedure TdnSplitter.SetButtonPosition(const Value: Integer);
begin
  if FButtonPosition <> Value then
  begin
    FButtonPosition := Value;
    if FButtonVisible then
      Invalidate;
  end;
end;

procedure TdnSplitter.SetButtonWidth(const Value: integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    if (FButtonWidthType = btwPercentage) and (FButtonWidth > 100) then
      FButtonWidth := 100;
    if FButtonWidth < 0 then
      FButtonWidth := 0;

    if FButtonVisible then
      Invalidate;
  end;
end;

procedure TdnSplitter.SetButtonWidthType(const Value: TdnButtonWidthType);
begin
  if FButtonWidthType <> Value then
  begin
    FButtonWidthType := Value;
    if (FButtonWidthType = btwPercentage) and (FButtonWidth > 100) then
      FButtonWidth := 100;

    if FButtonVisible then
      Invalidate;
  end;
end;

function TdnSplitter.GetButtonColor(Index: Integer): TColor;
begin
  Result := FButtonColors[Index];
end;

procedure TdnSplitter.SetButtonColor(Index: Integer; const Value: TColor);
begin
  if FButtonColors[Index] <> Value then
  begin
    FButtonColors[Index] := Value;
    if FButtonVisible and not FPainting then
      Invalidate;
  end;
end;

function TdnSplitter.GetButtonRect: TRect;
var
  BW, BP: Integer;
  X, Y: Integer;
begin
  // Calc the rectangle the button goes in
  if ButtonWidthType = btwPercentage then
  begin
    if Align in [alLeft, alRight] then
      BW := ClientRect.Bottom - ClientRect.Top
    else
      BW := ClientRect.Right - ClientRect.Left;
    BW := MulDiv(BW, FButtonWidth, 100);
  end else
    BW := FButtonWidth;

  if BW >= 1 then
  begin
    FButtonRect := ClientRect;

    if Align in [alLeft, alRight] then
    begin
      X := FButtonRect.Top;
      Y := FButtonRect.Bottom;
    end else
    begin
      X := FButtonRect.Left;
      Y := FButtonRect.Right;
    end;

    case ButtonAlign of
      baCenter:
        BP := ButtonPosition + (Y - X - BW) div 2;
      baRightBottom:
        BP := Y - BW - ButtonPosition;
    else  // baLeftTop
      BP := ButtonPosition;
    end;

    if Align in [alLeft, alRight] then
    begin
      FButtonRect.Top := BP;
      FButtonRect.Bottom := BP + BW;
    end else
    begin
      FButtonRect.Left := BP;
      FButtonRect.Right := BP + BW;
    end;
  end else
    SetRectEmpty(FButtonRect);

  Result := FButtonRect;
end;

procedure TdnSplitter.CMChildKey(var Message: TCMChildKey);
begin
  if FDraging and (Message.CharCode = VK_ESCAPE) then
  begin
    CancelDrag;
    Message.Result := 1;
  end else
    inherited;
end;

procedure TdnSplitter.CMEnabledChanged(var Message: TMessage);
begin
  inherited;

  Invalidate;
end;

procedure TdnSplitter.CMHintShow(var Message: TMessage);
begin
  inherited;

  // если кнопка отображается и не подсвечена - сбрасываем Hint
  if FDraging or (FButtonVisible and not FIsHighlighted) then
    Application.CancelHint;
end;

procedure TdnSplitter.CMMouseEnter(var Msg: TWMMouse);
var
  Pos: TPoint;
begin
  inherited;

  if FDraging then
    Exit;

  if not (csDesigning in ComponentState) then
  begin
    // CM_MOUSEENTER doesn't send mouse pos.
    GetCursorPos(Pos);
    Pos := ScreenToClient(Pos);
    CheckHighlighted(Pos);
  end;
end;

procedure TdnSplitter.CMMouseLeave(var Msg: TWMMouse);
begin
  inherited;

  if not (csDesigning in ComponentState) then
    CheckHighlighted(False);
end;

//procedure TdnSplitter.WMEraseBkgnd(var Message: TWmEraseBkgnd);
//begin
//  inherited;
//  //Message.Result := 1;
//end;

end.

