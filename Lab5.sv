`timescale 1ns / 1ns
`define MOVE_FIX 10'd8

package Lab5;
    typedef enum logic [1:0] { MOVE, DIE, SUCCESS } GameState;
    typedef logic [9:0] VGA_Position;
    parameter SPRITE_SIZE = 10'd64;
    parameter PLAYER_SIZE = 10'd48;
endpackage

interface Position;
    Lab5::VGA_Position X, Y;

    modport In(input X, Y);
    modport Out(output X, Y);
endinterface

interface ROM;
    logic [12:0] addr;
    logic [11:0] out;
endinterface

module main(VGA_R, VGA_G, VGA_B, H_Sync, V_Sync, Seg_L, Seg_En_L, LED, S4, S3, S0, FPGA_CLK, RESET);
    output [3:0] VGA_R, VGA_G, VGA_B;
    output H_Sync, V_Sync;
    output [7:0] Seg_L; 
    output [3:0] Seg_En_L; 
    output [15:0] LED;
    input S4, S3, S0, FPGA_CLK, RESET;
    logic isS4, isS3, isS0, CLK_25MHz, CLK_60Hz, CLK_25Hz, CLK_1Hz, CLK_05Hz, Touch;
    Lab5::GameState gameState;
    
    Position Player();
    Position Car[0:2]();
    Position Truck1[0:2]();
    Position Truck2[0:2]();
    Position BigTruck1[0:1]();
    Position BigTruck2[0:1]();
    Position BigTruck3[0:1]();
    Position Brick[0:4]();
    
    ClockDiv clock_div(.*);
    CLK_DIV clk_div(.clk_in1(FPGA_CLK), .CLK_25MHz(CLK_25MHz), .resetn(RESET));
    VGA_Display vga_display(.*);
    Seg_Display seg_display(.*);
    LED_Display led_display(.*);
    Player_Control player_control(.*);
    Car_Control car_control(.*);
    DeBounce debounceS4(isS4, S4, CLK_25Hz, RESET);
    DeBounce debounceS3(isS3, S3, CLK_25Hz, RESET);
    DeBounce debounceS0(isS0, S0, CLK_25Hz, RESET);
endmodule

module VGA_Display(VGA_R, VGA_G, VGA_B, H_Sync, V_Sync, Touch, Player, Car, Truck1, Truck2, BigTruck1, BigTruck2, BigTruck3, Brick, CLK_25MHz, RESET);
    output [3:0] VGA_R, VGA_G, VGA_B;
    output H_Sync, V_Sync, Touch;
    input Position.In Player;
    input Position.In Car[0:2];
    input Position.In Truck1[0:2];
    input Position.In Truck2[0:2];
    input Position.In BigTruck1[0:1];
    input Position.In BigTruck2[0:1];
    input Position.In BigTruck3[0:1];
    input Position.In Brick[0:4];
    input CLK_25MHz, RESET;

    ROM PlayerROM(), CarROM(), Truck1ROM(), Truck2ROM(), BigTruck1ROM(), BigTruck2ROM(), BigTruck3ROM(), BrickROM();
    Lab5::VGA_Position H_Cnt, V_Cnt;
    logic [11:0] vga_data;
    logic Player_Area;
    logic Car_Area[0:2], Truck1_Area[0:2], Truck2_Area[0:2];
    logic BigTruck1_Area[0:1], BigTruck2_Area[0:1], BigTruck3_Area[0:1], Brick_Area[0:4], Edge_Area, v_Line_Area, h_Line_Area;
    logic dataValid;

    logic Touch_Car, Touch_Truck, Touch_BigTruck, Touch_Brick;

    SyncGeneration syncgeneration(.*);
    Player_Sprite player_sprite(.clka(CLK_25MHz), .addra(PlayerROM.addr), .douta(PlayerROM.out));
    Car_Sprite car_sprite(.clka(CLK_25MHz), .addra(CarROM.addr), .douta(CarROM.out));
    Truck1_Sprite truck1_sprite(.clka(CLK_25MHz), .addra(Truck1ROM.addr), .douta(Truck1ROM.out));
    Truck2_Sprite truck2_sprite(.clka(CLK_25MHz), .addra(Truck2ROM.addr), .douta(Truck2ROM.out));
    BigTruck1_Sprite bigtruck1_sprite(.clka(CLK_25MHz), .addra(BigTruck1ROM.addr), .douta(BigTruck1ROM.out));
    BigTruck2_Sprite bigtruck2_sprite(.clka(CLK_25MHz), .addra(BigTruck2ROM.addr), .douta(BigTruck2ROM.out));
    BigTruck3_Sprite bigtruck3_sprite(.clka(CLK_25MHz), .addra(BigTruck3ROM.addr), .douta(BigTruck3ROM.out));
    Brick_Sprite brick_sprite(.clka(CLK_25MHz), .addra(BrickROM.addr), .douta(BrickROM.out));

    assign {VGA_R, VGA_G, VGA_B} = vga_data;

    assign Touch = Player_Area & ((Touch_Car | Touch_Truck) | (Touch_BigTruck | Touch_Brick));
    assign Touch_Car = (Car_Area[0] | Car_Area[1] | Car_Area[2]);
    assign Touch_Truck = ((Truck1_Area[0] | Truck1_Area[1] | Truck1_Area[2]) | (Truck2_Area[0] | Truck2_Area[1] | Truck2_Area[2]));
    assign Touch_BigTruck = ((BigTruck1_Area[0] | BigTruck1_Area[1]) | (BigTruck2_Area[0] | BigTruck2_Area[1]) | (BigTruck3_Area[0] | BigTruck3_Area[1]));
    assign Touch_Brick = ((Brick_Area[0] | Brick_Area[1]) | Brick_Area[2] | (Brick_Area[3] | Brick_Area[4]));

    assign Player_Area = (((H_Cnt + 10'd1) >= Player.X) & ((H_Cnt + 10'd1) <= Player.X + Lab5::PLAYER_SIZE) & (V_Cnt >= Player.Y) & (V_Cnt < Player.Y + Lab5::PLAYER_SIZE)) ? 1'b1 : 1'b0;
    Area_Detector car_area0_detector(Car_Area[0], Car[0], H_Cnt, V_Cnt);
    Area_Detector car_area1_detector(Car_Area[1], Car[1], H_Cnt, V_Cnt);
    Area_Detector car_area2_detector(Car_Area[2], Car[2], H_Cnt, V_Cnt);
    Area_Detector truck1_area0_detector(Truck1_Area[0], Truck1[0], H_Cnt, V_Cnt);
    Area_Detector truck1_area1_detector(Truck1_Area[1], Truck1[1], H_Cnt, V_Cnt);
    Area_Detector truck1_area2_detector(Truck1_Area[2], Truck1[2], H_Cnt, V_Cnt);
    Area_Detector truck2_area0_detector(Truck2_Area[0], Truck2[0], H_Cnt, V_Cnt);
    Area_Detector truck2_area1_detector(Truck2_Area[1], Truck2[1], H_Cnt, V_Cnt);
    Area_Detector truck2_area2_detector(Truck2_Area[2], Truck2[2], H_Cnt, V_Cnt);

    assign BigTruck1_Area[0] = (((H_Cnt + 10'd1) >= BigTruck1[0].X) & ((H_Cnt + 10'd1) <= BigTruck1[0].X + 10'd56) & (V_Cnt >= BigTruck1[0].Y) & (V_Cnt < BigTruck1[0].Y + Lab5::SPRITE_SIZE)) ? 1'b1 : 1'b0;
    assign BigTruck1_Area[1] = (((H_Cnt + 10'd1) >= BigTruck1[1].X) & ((H_Cnt + 10'd1) <= BigTruck1[1].X + 10'd56) & (V_Cnt >= BigTruck1[1].Y) & (V_Cnt < BigTruck1[1].Y + Lab5::SPRITE_SIZE)) ? 1'b1 : 1'b0;
    assign BigTruck2_Area[0] = (((H_Cnt + 10'd1) >= BigTruck2[0].X) & ((H_Cnt + 10'd1) <= BigTruck2[0].X + 10'd80) & (V_Cnt >= BigTruck2[0].Y) & (V_Cnt < BigTruck2[0].Y + Lab5::SPRITE_SIZE)) ? 1'b1 : 1'b0;
    assign BigTruck2_Area[1] = (((H_Cnt + 10'd1) >= BigTruck2[1].X) & ((H_Cnt + 10'd1) <= BigTruck2[1].X + 10'd80) & (V_Cnt >= BigTruck2[1].Y) & (V_Cnt < BigTruck2[1].Y + Lab5::SPRITE_SIZE)) ? 1'b1 : 1'b0;
    assign BigTruck3_Area[0] = (((H_Cnt + 10'd1) >= BigTruck3[0].X) & ((H_Cnt + 10'd1) <= BigTruck3[0].X + 10'd56) & (V_Cnt >= BigTruck3[0].Y) & (V_Cnt < BigTruck3[0].Y + Lab5::SPRITE_SIZE)) ? 1'b1 : 1'b0;
    assign BigTruck3_Area[1] = (((H_Cnt + 10'd1) >= BigTruck3[1].X) & ((H_Cnt + 10'd1) <= BigTruck3[1].X + 10'd56) & (V_Cnt >= BigTruck3[1].Y) & (V_Cnt < BigTruck3[1].Y + Lab5::SPRITE_SIZE)) ? 1'b1 : 1'b0;

    Area_Detector brick_area0_detector(Brick_Area[0], Brick[0], H_Cnt, V_Cnt);
    Area_Detector brick_area1_detector(Brick_Area[1], Brick[1], H_Cnt, V_Cnt);
    Area_Detector brick_area2_detector(Brick_Area[2], Brick[2], H_Cnt, V_Cnt);
    Area_Detector brick_area3_detector(Brick_Area[3], Brick[3], H_Cnt, V_Cnt);
    Area_Detector brick_area4_detector(Brick_Area[4], Brick[4], H_Cnt, V_Cnt);

    assign Edge_Area = (V_Cnt >= 10'd0 && V_Cnt <= 10'd5) | (V_Cnt >= 10'd475 && V_Cnt <= 10'd480) | (H_Cnt >= 10'd0 && H_Cnt <= 10'd5) | (H_Cnt >= 10'd635 && H_Cnt <= 10'd640);
    assign v_Line_Area = (H_Cnt >= 10'd79 && H_Cnt <= 10'd81) | (H_Cnt >= 10'd159 && H_Cnt <= 10'd161) | (H_Cnt >= 10'd239 && H_Cnt <= 10'd241) | (H_Cnt >= 10'd319 && H_Cnt <= 10'd321)
                        | (H_Cnt >= 10'd399 && H_Cnt <= 10'd401) | (H_Cnt >= 10'd479 && H_Cnt <= 10'd481) | (H_Cnt >= 10'd559 && H_Cnt <= 10'd561);
    assign h_Line_Area = (V_Cnt >= 10'd79 && V_Cnt <= 10'd81) | (V_Cnt >= 10'd159 && V_Cnt <= 10'd161) | (V_Cnt >= 10'd239 && V_Cnt <= 10'd241) 
                        | (V_Cnt >= 10'd319 && V_Cnt <= 10'd321) | (V_Cnt >= 10'd399 && V_Cnt <= 10'd401);

    assign PlayerROM.addr = (dataValid & Player_Area) ? (V_Cnt - Player.Y) * (Lab5::PLAYER_SIZE) + ((H_Cnt + 10'd1) - Player.X) : 13'd0;
    
    always_comb begin
        if(dataValid) begin
            if(Car_Area[0]) CarROM.addr = (V_Cnt - Car[0].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Car[0].X);
            else if(Car_Area[1]) CarROM.addr = (V_Cnt - Car[1].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Car[1].X);
            else if(Car_Area[2]) CarROM.addr = (V_Cnt - Car[2].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Car[2].X);
            else CarROM.addr = 13'd0;
        end
        else CarROM.addr = 13'd0;
    end

    always_comb begin
        if(dataValid) begin
            if(Truck2_Area[0]) Truck2ROM.addr = (V_Cnt - Truck2[0].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Truck2[0].X);
            else if(Truck2_Area[1]) Truck2ROM.addr = (V_Cnt - Truck2[1].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Truck2[1].X);
            else if(Truck2_Area[2]) Truck2ROM.addr = (V_Cnt - Truck2[2].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Truck2[2].X);
            else Truck2ROM.addr = 13'd0;
        end
        else Truck2ROM.addr = 13'd0;
    end

    always_comb begin
        if(dataValid) begin
            if(Truck1_Area[0]) Truck1ROM.addr = (V_Cnt - Truck1[0].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Truck1[0].X);
            else if(Truck1_Area[1]) Truck1ROM.addr = (V_Cnt - Truck1[1].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Truck1[1].X);
            else if(Truck1_Area[2]) Truck1ROM.addr = (V_Cnt - Truck1[2].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Truck1[2].X);
            else Truck1ROM.addr = 13'd0;
        end
        else Truck1ROM.addr = 13'd0;
    end

    always_comb begin
        if(dataValid) begin
            if(BigTruck3_Area[0]) BigTruck3ROM.addr = (V_Cnt - BigTruck3[0].Y) * (10'd56) + ((H_Cnt + 10'd1) - BigTruck3[0].X);
            else if(BigTruck3_Area[1]) BigTruck3ROM.addr = (V_Cnt - BigTruck3[1].Y) * (10'd56) + ((H_Cnt + 10'd1) - BigTruck3[1].X);
            else BigTruck3ROM.addr = 13'd0;
        end
        else BigTruck3ROM.addr = 13'd0;
    end

    always_comb begin
        if(dataValid) begin
            if(BigTruck2_Area[0]) BigTruck2ROM.addr = (V_Cnt - BigTruck2[0].Y) * (10'd80) + ((H_Cnt + 10'd1) - BigTruck2[0].X);
            else if(BigTruck2_Area[1]) BigTruck2ROM.addr = (V_Cnt - BigTruck2[1].Y) * (10'd80) + ((H_Cnt + 10'd1) - BigTruck2[1].X);
            else BigTruck2ROM.addr = 13'd0;
        end
        else BigTruck2ROM.addr = 13'd0;
    end

    always_comb begin
        if(dataValid) begin
            if(BigTruck1_Area[0]) BigTruck1ROM.addr = (V_Cnt - BigTruck1[0].Y) * (10'd56) + ((H_Cnt + 10'd1) - BigTruck1[0].X);
            else if(BigTruck1_Area[1]) BigTruck1ROM.addr = (V_Cnt - BigTruck1[1].Y) * (10'd56) + ((H_Cnt + 10'd1) - BigTruck1[1].X);
            else BigTruck1ROM.addr = 13'd0;
        end
        else BigTruck1ROM.addr = 13'd0;
    end

    always_comb begin
        if(dataValid) begin
            if(Brick_Area[0]) BrickROM.addr = (V_Cnt - Brick[0].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Brick[0].X);
            else if(Brick_Area[1]) BrickROM.addr = (V_Cnt - Brick[1].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Brick[1].X);
            else if(Brick_Area[2]) BrickROM.addr = (V_Cnt - Brick[2].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Brick[2].X);
            else if(Brick_Area[3]) BrickROM.addr = (V_Cnt - Brick[3].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Brick[3].X);
            else if(Brick_Area[4]) BrickROM.addr = (V_Cnt - Brick[4].Y) * (Lab5::SPRITE_SIZE) + ((H_Cnt + 10'd1) - Brick[4].X);
            else BrickROM.addr = 13'd0;
        end
        else BrickROM.addr = 13'd0;
    end

    always_ff@(posedge CLK_25MHz or negedge RESET) begin
        if(!RESET) vga_data <= 12'h000;   
        else begin
            if(dataValid) begin
                if(Edge_Area) vga_data <= 12'h00f;
                else if(Player_Area) vga_data <= PlayerROM.out;
                else if(Car_Area[0]) vga_data <= CarROM.out;
                else if(Car_Area[1]) vga_data <= CarROM.out;
                else if(Car_Area[2]) vga_data <= CarROM.out;
                else if(Truck1_Area[0]) vga_data <= Truck1ROM.out;
                else if(Truck1_Area[1]) vga_data <= Truck1ROM.out;
                else if(Truck1_Area[2]) vga_data <= Truck1ROM.out;
                else if(Truck2_Area[0]) vga_data <= Truck2ROM.out;
                else if(Truck2_Area[1]) vga_data <= Truck2ROM.out;
                else if(Truck2_Area[2]) vga_data <= Truck2ROM.out;
                else if(BigTruck1_Area[0]) vga_data <= BigTruck1ROM.out;
                else if(BigTruck1_Area[1]) vga_data <= BigTruck1ROM.out;
                else if(BigTruck2_Area[0]) vga_data <= BigTruck2ROM.out;
                else if(BigTruck2_Area[1]) vga_data <= BigTruck2ROM.out;
                else if(BigTruck3_Area[0]) vga_data <= BigTruck3ROM.out;
                else if(BigTruck3_Area[1]) vga_data <= BigTruck3ROM.out;
                else if(Brick_Area[0]) vga_data <= BrickROM.out;
                else if(Brick_Area[1]) vga_data <= BrickROM.out;
                else if(Brick_Area[2]) vga_data <= BrickROM.out;
                else if(Brick_Area[3]) vga_data <= BrickROM.out;
                else if(Brick_Area[4]) vga_data <= BrickROM.out;
                else begin
                    if(v_Line_Area) begin
                        if(V_Cnt[3:0] <= 3'd6) vga_data <= 12'hfff;
                        else vga_data <= 12'h000;
                    end
                    else if(h_Line_Area) begin
                        if(H_Cnt[3:0] <= 3'd6) vga_data <= 12'hfff;
                        else vga_data <= 12'h000;
                    end
                    else if((V_Cnt >= 10'd240 && V_Cnt <= 10'd400)) vga_data <= 12'hf80;
                    else if((V_Cnt >= 10'd80 && V_Cnt <= 10'd160)) vga_data <= 12'h08f;
                    else vga_data <= 12'hfff;
                end
            end
            else vga_data <= 12'h000;
        end
    end
endmodule

module Area_Detector(isInArea, Sprite, H_Cnt, V_Cnt);
    output isInArea;
    input Position.In Sprite;
    input Lab5::VGA_Position H_Cnt, V_Cnt;

    assign isInArea = (((H_Cnt + 10'd1) >= Sprite.X) & ((H_Cnt + 10'd1) <= Sprite.X + Lab5::SPRITE_SIZE) & (V_Cnt >= Sprite.Y) & (V_Cnt < Sprite.Y + Lab5::SPRITE_SIZE)) ? 1'b1 : 1'b0;
endmodule

module Seg_Display(Seg_L, Seg_En_L, isS4, isS3, isS0, gameState, CLK_25Hz, CLK_60Hz, RESET);
    output logic [7:0] Seg_L; 
    output logic [3:0] Seg_En_L; 
    input isS4, isS3, isS0, CLK_25Hz, CLK_60Hz, RESET;
    input Lab5::GameState gameState;
    logic [7:0] Step;
    logic [3:0] Seg_value;
    
    always_ff@(posedge CLK_60Hz or negedge RESET) begin : Seg_Enable_Control
        if(!RESET) Seg_En_L <= 4'b1000;
        else Seg_En_L <= {Seg_En_L[0], Seg_En_L[3:1]};
    end

    always_ff@(posedge CLK_25Hz or negedge RESET) begin
        if(!RESET) Step <= 8'd0;
        else begin
            if((isS4 || isS3 || isS0) & (gameState == Lab5::MOVE)) begin
                if(Step[3:0] == 4'd9) Step <= {Step[7:4] + 4'd1, 4'd0};
                else Step <= Step + 8'd1;
            end
            else Step <= Step;
        end
    end

    always_comb begin
        case(Seg_En_L)
            4'b1000: Seg_value = (gameState == Lab5::SUCCESS) ? 4'd9 : 4'd15;
            4'b0100: Seg_value = (gameState == Lab5::SUCCESS) ? 4'd9 : 4'd15;
            4'b0010: Seg_value = Step[7:4];
            4'b0001: Seg_value = Step[3:0];
            default: Seg_value = 4'd15;
        endcase
    end

    always_comb begin : left_segment_Logic
        case(Seg_value)
            4'd0: Seg_L = 8'b11111100;
            4'd1: Seg_L = 8'b01100000;
            4'd2: Seg_L = 8'b11011010;
            4'd3: Seg_L = 8'b11110010;
            4'd4: Seg_L = 8'b01100110;
            4'd5: Seg_L = 8'b10110110;
            4'd6: Seg_L = 8'b10111110;
            4'd7: Seg_L = 8'b11100100;
            4'd8: Seg_L = 8'b11111110;
            4'd9: Seg_L = 8'b11110110;
            default: Seg_L = 8'b00000000; 
        endcase
    end
endmodule

module LED_Display(LED, gameState, CLK_25MHz, CLK_1Hz, RESET, Touch, Player);
    output logic [15:0] LED;
    output Lab5::GameState gameState;
    input CLK_25MHz, CLK_1Hz, RESET, Touch;
    input Position.In Player;

    logic Arrive;

    assign Arrive = (Player.Y <= 10'd80) & ((Player.X >= 10'd80 & Player.X <= 10'd160) | (Player.X >= 10'd320 & Player.X <= 10'd400) | (Player.X >= 10'd560 & Player.X <= 10'd640));

    always_ff@(posedge CLK_25MHz or negedge RESET) begin
        if(!RESET) gameState <= Lab5::MOVE;
        else begin
            if(gameState == Lab5::MOVE) begin
                if(Touch) gameState <= Lab5::DIE;
                else if(Arrive) gameState <= Lab5::SUCCESS;
                else gameState <= gameState;
            end
            else gameState <= gameState;
        end
    end

    always_ff@(posedge CLK_1Hz or negedge RESET) begin
        if(!RESET) LED <= 16'b0000_0000_0000_0000;
        else begin
            if(gameState == Lab5::DIE && LED == 16'b0000_0000_0000_0000) LED <= 16'b0000_0001_1000_0000;
            else begin
                LED[15:8] <= {LED[14:8], LED[15]};
                LED[7:0] <= {LED[0], LED[7:1]};
            end
        end
    end
endmodule

module Player_Control(Player, Brick, isS4, isS3, isS0, gameState, CLK_25Hz, RESET);
    output Position.Out Player;
    output Position.Out Brick[0:4];
    input isS4, isS3, isS0, CLK_25Hz, RESET;
    input Lab5::GameState gameState;

    assign {Brick[0].X, Brick[1].X, Brick[2].X, Brick[3].X, Brick[4].X} = {10'd0 + `MOVE_FIX, 10'd160 + `MOVE_FIX, 10'd240 + `MOVE_FIX, 10'd400 + `MOVE_FIX, 10'd480 + `MOVE_FIX};
    assign {Brick[0].Y, Brick[1].Y, Brick[2].Y, Brick[3].Y, Brick[4].Y} = {5{10'd0 + `MOVE_FIX}};

    always_ff@(posedge CLK_25Hz or negedge RESET) begin
        if(!RESET) {Player.X, Player.Y} <= {10'd80 + 10'd16, 10'd400 + 10'd16};
        else begin
            if(gameState == Lab5::MOVE) begin
                if(isS4 && Player.Y > 10'd80) {Player.X, Player.Y} <= {Player.X, Player.Y - 10'd80};
                else if(isS3 && Player.X > 10'd80) {Player.X, Player.Y} <= {Player.X - 10'd80, Player.Y};
                else if(isS0 && Player.X < 10'd560) {Player.X, Player.Y} <= {Player.X + 10'd80, Player.Y};
                else {Player.X, Player.Y} <= {Player.X, Player.Y};
            end
            else {Player.X, Player.Y} <= {Player.X, Player.Y};
        end
    end
endmodule

module Car_Control(Player, Car, Truck1, Truck2, BigTruck1, BigTruck2, BigTruck3, gameState, CLK_1Hz, CLK_05Hz, RESET);
    output Position.Out Player, Car[0:2], Truck1[0:2], Truck2[0:2], BigTruck1[0:1], BigTruck2[0:1], BigTruck3[0:1];
    input CLK_1Hz, CLK_05Hz, RESET;
    input Lab5::GameState gameState;

    always_ff@(posedge CLK_1Hz or negedge RESET) begin
        if(!RESET) {Car[0].X, Car[0].Y} <= {10'd0 + `MOVE_FIX, 10'd320 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(Car[0].X >= 10'd640) {Car[0].X, Car[0].Y} <= {10'd0 + `MOVE_FIX, 10'd320 + `MOVE_FIX};
            else {Car[0].X, Car[0].Y} <= {Car[0].X + 10'd80, 10'd320 + `MOVE_FIX};
        end
        else {Car[0].X, Car[0].Y} <= {Car[0].X, Car[0].Y};
    end
    always_ff@(posedge CLK_1Hz or negedge RESET) begin
        if(!RESET) {Car[1].X, Car[1].Y} <= {10'd240 + `MOVE_FIX, 10'd320 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(Car[1].X >= 10'd640) {Car[1].X, Car[1].Y} <= {10'd0 + `MOVE_FIX, 10'd320 + `MOVE_FIX};
            else {Car[1].X, Car[1].Y} <= {Car[1].X + 10'd80, 10'd320 + `MOVE_FIX};
        end
        else {Car[1].X, Car[1].Y} <= {Car[1].X, Car[1].Y};
    end
    always_ff@(posedge CLK_1Hz or negedge RESET) begin
        if(!RESET) {Car[2].X, Car[2].Y} <= {10'd480 + `MOVE_FIX, 10'd320 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(Car[2].X >= 10'd640) {Car[2].X, Car[2].Y} <= {10'd0 + `MOVE_FIX, 10'd320 + `MOVE_FIX};
            else {Car[2].X, Car[2].Y} <= {Car[2].X + 10'd80, 10'd320 + `MOVE_FIX};
        end
        else {Car[2].X, Car[2].Y} <= {Car[2].X, Car[2].Y};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {Truck1[0].X, Truck1[0].Y} <= {10'd1 + (`MOVE_FIX * 2), 10'd240 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(Truck1[0].X >= 10'd640) {Truck1[0].X, Truck1[0].Y} <= {10'd1 + (`MOVE_FIX * 2), 10'd240 + `MOVE_FIX};
            else {Truck1[0].X, Truck1[0].Y} <= {Truck1[0].X + 10'd80, 10'd240 + `MOVE_FIX};
        end
        else {Truck1[0].X, Truck1[0].Y} <= {Truck1[0].X, Truck1[0].Y};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {Truck1[1].X, Truck1[1].Y} <= {10'd241 + (`MOVE_FIX * 2), 10'd240 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(Truck1[1].X >= 10'd640) {Truck1[1].X, Truck1[1].Y} <= {10'd1 + (`MOVE_FIX * 2), 10'd240 + `MOVE_FIX};
            else {Truck1[1].X, Truck1[1].Y} <= {Truck1[1].X + 10'd80, 10'd240 + `MOVE_FIX};
        end
        else {Truck1[1].X, Truck1[1].Y} <= {Truck1[1].X, Truck1[1].Y};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {Truck1[2].X, Truck1[2].Y} <= {10'd481 + (`MOVE_FIX * 2), 10'd240 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(Truck1[2].X >= 10'd640) {Truck1[2].X, Truck1[2].Y} <= {10'd1 + (`MOVE_FIX * 2), 10'd240 + `MOVE_FIX};
            else {Truck1[2].X, Truck1[2].Y} <= {Truck1[2].X + 10'd80, 10'd240 + `MOVE_FIX};
        end
        else {Truck1[2].X, Truck1[2].Y} <= {Truck1[2].X, Truck1[2].Y};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {Truck2[0].X, Truck2[0].Y} <= {10'd80, 10'd240 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(Truck2[0].X >= 10'd640) {Truck2[0].X, Truck2[0].Y} <= {10'd0, 10'd240 + `MOVE_FIX};
            else {Truck2[0].X, Truck2[0].Y} <= {Truck2[0].X + 10'd80, 10'd240 + `MOVE_FIX};
        end
        else {Truck2[0].X, Truck2[0].Y} <= {Truck2[0].X, Truck2[0].Y};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {Truck2[1].X, Truck2[1].Y} <= {10'd320, 10'd240 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(Truck2[1].X >= 10'd640) {Truck2[1].X, Truck2[1].Y} <= {10'd0, 10'd240 + `MOVE_FIX};
            else {Truck2[1].X, Truck2[1].Y} <= {Truck2[1].X + 10'd80, 10'd240 + `MOVE_FIX};
        end
        else {Truck2[1].X, Truck2[1].Y} <= {Truck2[1].X, Truck2[1].Y};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {Truck2[2].X, Truck2[2].Y} <= {10'd560, 10'd240 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(Truck2[2].X >= 10'd640) {Truck2[2].X, Truck2[2].Y} <= {10'd0, 10'd240 + `MOVE_FIX};
            else {Truck2[2].X, Truck2[2].Y} <= {Truck2[2].X + 10'd80, 10'd240 + `MOVE_FIX};
        end
        else {Truck2[2].X, Truck2[2].Y} <= {Truck2[2].X, Truck2[2].Y};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {BigTruck1[0].X, BigTruck1[0].Y} <= {10'd26, 10'd80 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(BigTruck1[0].X < 10'd80) {BigTruck1[0].X, BigTruck1[0].Y} <= {10'd746, 10'd80 + `MOVE_FIX};
            else {BigTruck1[0].X, BigTruck1[0].Y} <= {BigTruck1[0].X - 10'd80, 10'd80 + `MOVE_FIX};
        end
        else {BigTruck1[0].X, BigTruck1[0].Y} <= {BigTruck1[0].X, 10'd80 + `MOVE_FIX};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {BigTruck1[1].X, BigTruck1[1].Y} <= {10'd426, 10'd80 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(BigTruck1[1].X < 10'd80) {BigTruck1[1].X, BigTruck1[1].Y} <= {10'd746, 10'd80 + `MOVE_FIX};
            else {BigTruck1[1].X, BigTruck1[1].Y} <= {BigTruck1[1].X - 10'd80, 10'd80 + `MOVE_FIX};
        end
        else {BigTruck1[1].X, BigTruck1[1].Y} <= {BigTruck1[1].X, 10'd80 + `MOVE_FIX};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {BigTruck2[0].X, BigTruck2[0].Y} <= {10'd81, 10'd80 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(BigTruck2[0].X < 10'd80) {BigTruck2[0].X, BigTruck2[0].Y} <= {10'd721, 10'd80 + `MOVE_FIX};
            else {BigTruck2[0].X, BigTruck2[0].Y} <= {BigTruck2[0].X - 10'd80, 10'd80 + `MOVE_FIX};
        end
        else {BigTruck2[0].X, BigTruck2[0].Y} <= {BigTruck2[0].X, 10'd80 + `MOVE_FIX};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {BigTruck2[1].X, BigTruck2[1].Y} <= {10'd481, 10'd80 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(BigTruck2[1].X < 10'd80) {BigTruck2[1].X, BigTruck2[1].Y} <= {10'd721, 10'd80 + `MOVE_FIX};
            else {BigTruck2[1].X, BigTruck2[1].Y} <= {BigTruck2[1].X - 10'd80, 10'd80 + `MOVE_FIX};
        end
        else {BigTruck2[1].X, BigTruck2[1].Y} <= {BigTruck2[1].X, 10'd80 + `MOVE_FIX};
    end
    
    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {BigTruck3[0].X, BigTruck3[0].Y} <= {10'd160, 10'd80 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(BigTruck3[0].X < 10'd80) {BigTruck3[0].X, BigTruck3[0].Y} <= {10'd720, 10'd80 + `MOVE_FIX};
            else {BigTruck3[0].X, BigTruck3[0].Y} <= {BigTruck3[0].X - 10'd80, 10'd80 + `MOVE_FIX};
        end
        else {BigTruck3[0].X, BigTruck3[0].Y} <= {BigTruck3[0].X, 10'd80 + `MOVE_FIX};
    end

    always_ff@(posedge CLK_05Hz or negedge RESET) begin
        if(!RESET) {BigTruck3[1].X, BigTruck3[1].Y} <= {10'd560, 10'd80 + `MOVE_FIX};
        else if(gameState == Lab5::MOVE) begin
            if(BigTruck3[1].X < 10'd80) {BigTruck3[1].X, BigTruck3[1].Y} <= {10'd720, 10'd80 + `MOVE_FIX};
            else {BigTruck3[1].X, BigTruck3[1].Y} <= {BigTruck3[1].X - 10'd80, 10'd80 + `MOVE_FIX};
        end
        else {BigTruck3[1].X, BigTruck3[1].Y} <= {BigTruck3[1].X, 10'd80 + `MOVE_FIX};
    end
endmodule

module DeBounce(isButton, Button, CLK, RESET);
    output isButton;
    input Button, CLK, RESET;
    logic [2:0] buffers;

    assign isButton = (!buffers[2]) & buffers[1] & buffers[0];
    always_ff@(posedge CLK or negedge RESET) begin
        if(!RESET) buffers <= 3'b000;
        else buffers <= {buffers[1:0], Button};
    end
endmodule

module SyncGeneration(H_Sync, V_Sync, dataValid, H_Cnt, V_Cnt, CLK_25MHz, RESET);
    output H_Sync, V_Sync, dataValid;
    output Lab5::VGA_Position H_Cnt, V_Cnt;
    input CLK_25MHz, RESET;
    parameter H_SP_END = 96;
    parameter H_BP_END = 144;
    parameter H_FP_START = 785;
    parameter H_TOTAL = 800;
   
    parameter V_SP_END = 2;
    parameter V_BP_END = 35;
    parameter V_FP_START = 516;
    parameter V_TOTAL = 525;

    Lab5::VGA_Position x_cnt, y_cnt;
    logic h_valid, v_valid;
     
   always_ff@(posedge CLK_25MHz or negedge RESET) begin
      if (!RESET) x_cnt <= 10'd1;
      else begin
         if(x_cnt == H_TOTAL) x_cnt <= 10'd1;
         else x_cnt <= x_cnt + 1;
      end
   end
   
   always_ff@(posedge CLK_25MHz or negedge RESET) begin
      if(!RESET) y_cnt <= 10'd1;
      else begin
         if(y_cnt == V_TOTAL & x_cnt == H_TOTAL) y_cnt <= 1;
         else if (x_cnt == H_TOTAL) y_cnt <= y_cnt + 1;
         else y_cnt<=y_cnt;
      end
   end
   
   assign H_Sync = ((x_cnt > H_SP_END)) ? 1'b1 : 1'b0;
   assign V_Sync = ((y_cnt > V_SP_END)) ? 1'b1 : 1'b0;
   assign h_valid = ((x_cnt > H_BP_END) & (x_cnt <= H_FP_START)) ? 1'b1 : 1'b0;
   assign v_valid = ((y_cnt > V_BP_END) & (y_cnt <= V_FP_START)) ? 1'b1 : 1'b0;
   assign dataValid = ((h_valid == 1'b1) & (v_valid == 1'b1)) ? 1'b1 :  1'b0;
   assign H_Cnt = ((h_valid == 1'b1)) ? x_cnt - H_BP_END : 10'b0;
   assign V_Cnt = ((v_valid == 1'b1)) ? y_cnt - V_BP_END : 10'b0; 
endmodule

module ClockDiv(CLK_60Hz, CLK_25Hz, CLK_1Hz, CLK_05Hz, CLK_25MHz, RESET);
    output CLK_60Hz, CLK_25Hz, CLK_1Hz, CLK_05Hz;
    input CLK_25MHz, RESET;
    logic [25:0] counters;

    assign CLK_60Hz = counters[16];
    assign CLK_25Hz = counters[19];
    assign CLK_1Hz = counters[24];
    assign CLK_05Hz = counters[25];
    always_ff@(posedge CLK_25MHz or negedge RESET) begin
        if(!RESET) counters <= 26'd0;
        else counters <= counters + 26'd1;
    end
endmodule