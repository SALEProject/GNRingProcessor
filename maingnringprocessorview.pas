unit MainGNRingProcessorView;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
 DBGrids, ComCtrls, StdCtrls, DOM, XMLRead, XMLWrite, DateUtils, GNProcessor;

type

 { Tfrm_MainForm }

 Tfrm_MainForm = class(TForm)
  img: TImage;
  Panel1: TPanel;
  Panel2: TPanel;
  scroll: TScrollBox;
  Splitter1: TSplitter;
  tmr: TTimer;
  tv_Orders: TTreeView;
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
  procedure tmrTimer(Sender: TObject);
  procedure tv_OrdersChange(Sender: TObject; Node: TTreeNode);
  procedure tv_OrdersCollapsed(Sender: TObject; Node: TTreeNode);
  procedure tv_OrdersExpanded(Sender: TObject; Node: TTreeNode);
 private
  { private declarations }
 public
  { public declarations }

  processor: TGNProcessor;
  ID_Selected: integer;

  procedure Refresh_TreeOrders;
  procedure Paint_Orders;
 end;

var
 frm_MainForm: Tfrm_MainForm;

implementation

{$R *.lfm}

procedure Tfrm_MainForm.tmrTimer(Sender: TObject);
begin
 tv_Orders.OnChange:= nil;
 tv_Orders.OnExpanded:= nil;
 tv_Orders.OnCollapsed:= nil;

 processor.ReadStatus;
 Refresh_TreeOrders;
 Paint_Orders;

 tv_Orders.OnChange:= @tv_OrdersChange;
 tv_Orders.OnExpanded:= @tv_OrdersExpanded;
 tv_Orders.OnCollapsed:= @tv_OrdersCollapsed;
end;

procedure Tfrm_MainForm.FormCreate(Sender: TObject);
var StatusFileName: shortstring;
begin
 StatusFileName:= ParamStr(0);
 StatusFileName:= IncludeTrailingPathDelimiter(ExtractFilePath(StatusFileName)) + 'GNRingProcessor_status.xml';

 processor:= TGNProcessor.Create(StatusFileName);

end;

procedure Tfrm_MainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 tv_Orders.OnChange:= nil;
 tv_Orders.OnExpanded:= nil;
 tv_Orders.OnCollapsed:= nil;
end;

procedure Tfrm_MainForm.tv_OrdersChange(Sender: TObject; Node: TTreeNode);
var idx: integer;
begin
 if tv_Orders.Selected = nil then Exit;
 idx:= processor.FindOrderbyNodeData(pointer(tv_Orders.Selected));
 if idx > -1 then
  begin
   ID_Selected:= processor.Orders[idx].ID_Order;
   Paint_Orders;
  end;
end;

procedure Tfrm_MainForm.tv_OrdersCollapsed(Sender: TObject; Node: TTreeNode);
begin
 Paint_Orders;
end;

procedure Tfrm_MainForm.tv_OrdersExpanded(Sender: TObject; Node: TTreeNode);
begin
 Paint_Orders;
end;

procedure Tfrm_MainForm.Refresh_TreeOrders;
 var i, j, k: integer;
     node, node_match, node_matchdet: TTreeNode;
 begin
  tv_Orders.Items.Clear;

  for i:= 0 to processor.OrdersCount {Length(Orders)} - 1 do
   begin
    node:= tv_Orders.Items.AddChild(nil, Format('ID_Order: %d; Quantity: %f; Price: %f', [processor.Orders[i].ID_Order, processor.Orders[i].Quantity, processor.Orders[i].Price]));
    processor.Orders[i].NodeData:= pointer(node);

    for j:= 0 to Length(processor.Orders[i].matches) - 1 do
     begin
      node_match:= tv_Orders.Items.AddChild(node, 'ID_OrderMatch: ' + IntToStr(processor.Orders[i].matches[j].ID_OrderMatch));
      processor.Orders[i].matches[j].NodeData:= node_match;

      for k:= 0 to Length(processor.Orders[i].matches[j].Orders) - 1 do
       begin
        node_matchdet:= tv_Orders.Items.AddChild(node_match, 'ID_Order: ' + IntToStr(processor.Orders[i].matches[j].Orders[k]));
       end;
     end;
   end;
 end;

procedure Tfrm_MainForm.Paint_Orders;

 procedure DrawOrder(ID_Order: integer; ismatch: boolean; matchrow: integer);
  var x, y, w, h: integer;
      idx, row: integer;
      order: TOrderRec;
  begin
   idx:= processor.FindOrder(ID_Order);
   if idx = -1 then Exit;

   order:= processor.Orders[idx];
   row:= order.row;
   w:= round(order.EndDeliveryDate - order.StartDeliveryDate {+ 1qwe}) * 20;
   h:= 20;
   x:= round(Order.StartDeliveryDate - processor.MinDeliveryDate) * 20;
   y:= 20 + row * 20;

   case ismatch of
    false: case order.Direction of
            odir_Buy : img.Canvas.Brush.Color:= clGreen;
            odir_Sell: img.Canvas.Brush.Color:= clMaroon;
           end;
    true : case order.Direction of
            odir_Buy : img.Canvas.Brush.Color:= clLime;
            odir_Sell: img.Canvas.Brush.Color:= clRed;
           end;
   end;

   img.Canvas.FillRect(x, y, x + w, y + h);
   img.Canvas.Font.Color:= clWhite;
   img.Canvas.TextOut(x, y, 'ID_Order: ' + IntToStr(order.ID_Order));

   if ismatch then
    begin
     w:= round(order.EndDeliveryDate - order.StartDeliveryDate {+ 1qwe}) * 20;
     h:= 20;
     x:= round(Order.StartDeliveryDate - processor.MinDeliveryDate) * 20;
     y:= 20 + matchrow * 20;

     case order.Direction of
      odir_Buy : img.Canvas.Brush.Color:= clLime;
      odir_Sell: img.Canvas.Brush.Color:= clRed;
     end;

     img.Canvas.FillRect(x, y, x + w, y + h);
     img.Canvas.Font.Color:= clWhite;
     img.Canvas.TextOut(x, y, 'ID_Order: ' + IntToStr(order.ID_Order));
    end;
  end;

 var i, j, k: integer;
     x, y, w, h, row: integer;
     order: TOrderRec;
 begin
  img.Width:= round(processor.MaxDeliveryDate - processor.MinDeliveryDate {+ 1qwe}) * 20 + 1;
  img.Height:= 20 + (tv_Orders.Items.Count * 20) + 1;
  img.Picture.Bitmap.Width:= img.Width;
  img.Picture.Bitmap.Height:= img.Height;
  img.Picture.Bitmap.PixelFormat:= pf24bit;
  img.Canvas.Brush.Color:= clWhite;
  img.Canvas.FillRect(0, 0, img.Width, img.Height);

  img.Canvas.Pen.Color:= clSilver;
  for i:= 0 to round(processor.MaxDeliveryDate - processor.MinDeliveryDate) {+ 1qwe} do
   begin
    img.Canvas.MoveTo(i * 20, 0);
    img.Canvas.LineTo(i * 20, img.Height);

    img.Canvas.Font.Color:= clGray;
    img.Canvas.TextOut(i * 20 + 4, 2, IntToStr(DayOfTheMonth(processor.MinDeliveryDate + i)));
   end;

  for i:= 0 to tv_Orders.Items.Count do
   begin
    img.Canvas.MoveTo(0, i * 20);
    img.Canvas.LineTo(img.Width, i * 20);
   end;

  //  first draw first level rows
  row:= 0;
  for i:= 0 to processor.OrdersCount {Length(Orders)} - 1 do
   begin
    processor.Orders[i].row:= row;
    order:= processor.Orders[i];
    DrawOrder(order.ID_Order, false, 0);
    row += 1;

    if order.NodeData = nil then Continue;
    if TTreeNode(processor.Orders[i].NodeData).Expanded then
     for j:= 0 to Length(order.matches) - 1 do
      begin
       row += 1;
       if TTreeNode(order.matches[j].NodeData).Expanded then
        for k:= 0 to Length(order.matches[j].Orders) - 1 do
         begin
          row += 1;
         end;
      end;
   end;

  //  now draw expanded levels
  row:= 0;
  for i:= 0 to processor.OrdersCount {Length(Orders)} - 1 do
   begin
    order:= processor.Orders[i];
    row += 1;

    if order.NodeData = nil then continue;
    if TTreeNode(order.NodeData).Expanded then
     begin
      for j:= 0 to Length(order.matches) - 1 do
       begin
        row += 1;

        if TTreeNode(order.matches[j].NodeData).Expanded then
         begin
          for k:= 0 to Length(order.matches[j].Orders) - 1 do
           begin
            DrawOrder(order.matches[j].Orders[k], true, row);
            row += 1;
           end;
         end;
       end;
     end;

   end;

  if (tv_Orders.Selected <> nil) and (ID_Selected > 0) then
   begin
    i:= processor.FindOrder(ID_Selected);
    x:= round(processor.Orders[i].StartDeliveryDate - processor.MinDeliveryDate) * 20;
    img.Canvas.Pen.Color:= clYellow;
    img.Canvas.MoveTo(x, 0);
    img.Canvas.LineTo(x, img.Height);

    x:= round(processor.Orders[i].EndDeliveryDate - processor.MinDeliveryDate + 1) * 20;
    img.Canvas.Pen.Color:= clYellow;
    img.Canvas.MoveTo(x, 0);
    img.Canvas.LineTo(x, img.Height);
   end;
 end;

end.

