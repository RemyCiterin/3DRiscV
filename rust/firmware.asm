
./rust/target/riscv32im-unknown-none-elf/release/SuperOS:     file format elf32-littleriscv


Disassembly of section .text:

80000000 <_start>:
80000000:	00005297          	auipc	t0,0x5
80000004:	f1428293          	addi	t0,t0,-236 # 80004f14 <__bss_start>

80000008 <.Lpcrel_hi1>:
80000008:	00006317          	auipc	t1,0x6
8000000c:	f4030313          	addi	t1,t1,-192 # 80005f48 <__bss_end>
80000010:	0062f863          	bgeu	t0,t1,80000020 <.Lpcrel_hi2>

80000014 <.bss_zero_loop>:
80000014:	00028023          	sb	zero,0(t0)
80000018:	00128293          	addi	t0,t0,1
8000001c:	fe62ece3          	bltu	t0,t1,80000014 <.bss_zero_loop>

80000020 <.Lpcrel_hi2>:
80000020:	00006117          	auipc	sp,0x6
80000024:	f1010113          	addi	sp,sp,-240 # 80005f30 <_ZN7SuperOS6kalloc16KERNEL_ALLOCATOR17h0e08997bf1bee0d5E.llvm.14510610307159664504>
80000028:	61c010ef          	jal	80001644 <kernel_main>

8000002c <.infinite_loop>:
8000002c:	0000006f          	j	8000002c <.infinite_loop>

80000030 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E>:
80000030:	f9010113          	addi	sp,sp,-112
80000034:	06112623          	sw	ra,108(sp)
80000038:	06812423          	sw	s0,104(sp)
8000003c:	06912223          	sw	s1,100(sp)
80000040:	07212023          	sw	s2,96(sp)
80000044:	05312e23          	sw	s3,92(sp)
80000048:	05412c23          	sw	s4,88(sp)
8000004c:	05512a23          	sw	s5,84(sp)
80000050:	05612823          	sw	s6,80(sp)
80000054:	05712623          	sw	s7,76(sp)
80000058:	05812423          	sw	s8,72(sp)
8000005c:	05912223          	sw	s9,68(sp)
80000060:	05a12023          	sw	s10,64(sp)
80000064:	03b12e23          	sw	s11,60(sp)
80000068:	0005aa03          	lw	s4,0(a1)
8000006c:	032a5403          	lhu	s0,50(s4)
80000070:	00b00713          	li	a4,11
80000074:	00060993          	mv	s3,a2
80000078:	00050b93          	mv	s7,a0
8000007c:	04e47663          	bgeu	s0,a4,800000c8 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x98>
80000080:	0085aa83          	lw	s5,8(a1)
80000084:	0045ab03          	lw	s6,4(a1)
80000088:	004a0493          	addi	s1,s4,4
8000008c:	001a8513          	addi	a0,s5,1
80000090:	002a9913          	slli	s2,s5,0x2
80000094:	02a46063          	bltu	s0,a0,800000b4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x84>
80000098:	012485b3          	add	a1,s1,s2
8000009c:	00251513          	slli	a0,a0,0x2
800000a0:	00a48533          	add	a0,s1,a0
800000a4:	41540633          	sub	a2,s0,s5
800000a8:	00261613          	slli	a2,a2,0x2
800000ac:	00004097          	auipc	ra,0x4
800000b0:	bec080e7          	jalr	-1044(ra) # 80003c98 <memmove>
800000b4:	00140413          	addi	s0,s0,1
800000b8:	012484b3          	add	s1,s1,s2
800000bc:	0134a023          	sw	s3,0(s1)
800000c0:	028a1923          	sh	s0,50(s4)
800000c4:	4100006f          	j	800004d4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x4a4>
800000c8:	00068913          	mv	s2,a3
800000cc:	0085aa83          	lw	s5,8(a1)
800000d0:	0045ab03          	lw	s6,4(a1)
800000d4:	80005537          	lui	a0,0x80005
800000d8:	f2e54003          	lbu	zero,-210(a0) # 80004f2e <__rust_no_alloc_shim_is_unstable>
800000dc:	80006537          	lui	a0,0x80006
800000e0:	f3050513          	addi	a0,a0,-208 # 80005f30 <_ZN7SuperOS6kalloc16KERNEL_ALLOCATOR17h0e08997bf1bee0d5E.llvm.14510610307159664504>
800000e4:	00400593          	li	a1,4
800000e8:	03400613          	li	a2,52
800000ec:	00001097          	auipc	ra,0x1
800000f0:	a3c080e7          	jalr	-1476(ra) # 80000b28 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E>
800000f4:	52050663          	beqz	a0,80000620 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x5f0>
800000f8:	00050493          	mv	s1,a0
800000fc:	00052023          	sw	zero,0(a0)
80000100:	00500513          	li	a0,5
80000104:	02049923          	sh	zero,50(s1)
80000108:	01712c23          	sw	s7,24(sp)
8000010c:	01212623          	sw	s2,12(sp)
80000110:	02aaf463          	bgeu	s5,a0,80000138 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x108>
80000114:	032a5503          	lhu	a0,50(s4)
80000118:	ffb50513          	addi	a0,a0,-5
8000011c:	00c00593          	li	a1,12
80000120:	02a49923          	sh	a0,50(s1)
80000124:	4ab57863          	bgeu	a0,a1,800005d4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x5a4>
80000128:	014a0593          	addi	a1,s4,20
8000012c:	00400913          	li	s2,4
80000130:	01800613          	li	a2,24
80000134:	0600006f          	j	80000194 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x164>
80000138:	02aa8c63          	beq	s5,a0,80000170 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x140>
8000013c:	00600513          	li	a0,6
80000140:	06aa9063          	bne	s5,a0,800001a0 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x170>
80000144:	032a5503          	lhu	a0,50(s4)
80000148:	ffa50513          	addi	a0,a0,-6
8000014c:	00c00593          	li	a1,12
80000150:	02a49923          	sh	a0,50(s1)
80000154:	48b57063          	bgeu	a0,a1,800005d4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x5a4>
80000158:	00000a93          	li	s5,0
8000015c:	00012823          	sw	zero,16(sp)
80000160:	018a0593          	addi	a1,s4,24
80000164:	00500913          	li	s2,5
80000168:	01c00613          	li	a2,28
8000016c:	05c0006f          	j	800001c8 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x198>
80000170:	032a5503          	lhu	a0,50(s4)
80000174:	ffa50513          	addi	a0,a0,-6
80000178:	00c00593          	li	a1,12
8000017c:	02a49923          	sh	a0,50(s1)
80000180:	44b57a63          	bgeu	a0,a1,800005d4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x5a4>
80000184:	018a0593          	addi	a1,s4,24
80000188:	00500913          	li	s2,5
8000018c:	01c00613          	li	a2,28
80000190:	00500a93          	li	s5,5
80000194:	01612823          	sw	s6,16(sp)
80000198:	000a0c93          	mv	s9,s4
8000019c:	0300006f          	j	800001cc <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x19c>
800001a0:	032a5503          	lhu	a0,50(s4)
800001a4:	ff950513          	addi	a0,a0,-7
800001a8:	00c00593          	li	a1,12
800001ac:	02a49923          	sh	a0,50(s1)
800001b0:	42b57263          	bgeu	a0,a1,800005d4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x5a4>
800001b4:	00012823          	sw	zero,16(sp)
800001b8:	01ca0593          	addi	a1,s4,28
800001bc:	ff9a8a93          	addi	s5,s5,-7
800001c0:	00600913          	li	s2,6
800001c4:	02000613          	li	a2,32
800001c8:	00048c93          	mv	s9,s1
800001cc:	0005ac03          	lw	s8,0(a1)
800001d0:	00448693          	addi	a3,s1,4
800001d4:	00ca05b3          	add	a1,s4,a2
800001d8:	00251613          	slli	a2,a0,0x2
800001dc:	00068513          	mv	a0,a3
800001e0:	00004097          	auipc	ra,0x4
800001e4:	9a8080e7          	jalr	-1624(ra) # 80003b88 <memcpy>
800001e8:	032a1923          	sh	s2,50(s4)
800001ec:	032cd403          	lhu	s0,50(s9)
800001f0:	004c8913          	addi	s2,s9,4
800001f4:	002a9b93          	slli	s7,s5,0x2
800001f8:	008afe63          	bgeu	s5,s0,80000214 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x1e4>
800001fc:	017905b3          	add	a1,s2,s7
80000200:	00458513          	addi	a0,a1,4
80000204:	41540633          	sub	a2,s0,s5
80000208:	00261613          	slli	a2,a2,0x2
8000020c:	00004097          	auipc	ra,0x4
80000210:	a8c080e7          	jalr	-1396(ra) # 80003c98 <memmove>
80000214:	00140413          	addi	s0,s0,1
80000218:	01790933          	add	s2,s2,s7
8000021c:	01392023          	sw	s3,0(s2)
80000220:	01912a23          	sw	s9,20(sp)
80000224:	028c9923          	sh	s0,50(s9)
80000228:	000a2983          	lw	s3,0(s4)
8000022c:	20098263          	beqz	s3,80000430 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x400>
80000230:	00000d93          	li	s11,0
80000234:	00500d13          	li	s10,5
80000238:	00200913          	li	s2,2
8000023c:	0240006f          	j	80000260 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x230>
80000240:	02812a03          	lw	s4,40(sp)
80000244:	280a0263          	beqz	s4,800004c8 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x498>
80000248:	02c12b03          	lw	s6,44(sp)
8000024c:	03012483          	lw	s1,48(sp)
80000250:	03412d83          	lw	s11,52(sp)
80000254:	03812c03          	lw	s8,56(sp)
80000258:	000a2983          	lw	s3,0(s4)
8000025c:	1c098c63          	beqz	s3,80000434 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x404>
80000260:	35bb1c63          	bne	s6,s11,800005b8 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x588>
80000264:	0329dd83          	lhu	s11,50(s3)
80000268:	030a5a03          	lhu	s4,48(s4)
8000026c:	00b00513          	li	a0,11
80000270:	2aade663          	bltu	s11,a0,8000051c <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x4ec>
80000274:	001b0b13          	addi	s6,s6,1
80000278:	03aa7663          	bgeu	s4,s10,800002a4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x274>
8000027c:	01312e23          	sw	s3,28(sp)
80000280:	03612023          	sw	s6,32(sp)
80000284:	00400513          	li	a0,4
80000288:	02a12223          	sw	a0,36(sp)
8000028c:	02810513          	addi	a0,sp,40
80000290:	01c10593          	addi	a1,sp,28
80000294:	00000097          	auipc	ra,0x0
80000298:	3bc080e7          	jalr	956(ra) # 80000650 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E>
8000029c:	02812b03          	lw	s6,40(sp)
800002a0:	0b80006f          	j	80000358 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x328>
800002a4:	03aa0063          	beq	s4,s10,800002c4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x294>
800002a8:	00600513          	li	a0,6
800002ac:	08aa1463          	bne	s4,a0,80000334 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x304>
800002b0:	00000a13          	li	s4,0
800002b4:	01312e23          	sw	s3,28(sp)
800002b8:	03612023          	sw	s6,32(sp)
800002bc:	03a12223          	sw	s10,36(sp)
800002c0:	0840006f          	j	80000344 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x314>
800002c4:	01312e23          	sw	s3,28(sp)
800002c8:	03612023          	sw	s6,32(sp)
800002cc:	03a12223          	sw	s10,36(sp)
800002d0:	02810513          	addi	a0,sp,40
800002d4:	01c10593          	addi	a1,sp,28
800002d8:	00000097          	auipc	ra,0x0
800002dc:	378080e7          	jalr	888(ra) # 80000650 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E>
800002e0:	02812a03          	lw	s4,40(sp)
800002e4:	032a5b03          	lhu	s6,50(s4)
800002e8:	001b0413          	addi	s0,s6,1
800002ec:	00600513          	li	a0,6
800002f0:	10ab6263          	bltu	s6,a0,800003f4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x3c4>
800002f4:	018a0593          	addi	a1,s4,24
800002f8:	01ca0513          	addi	a0,s4,28
800002fc:	002b1993          	slli	s3,s6,0x2
80000300:	fec98993          	addi	s3,s3,-20
80000304:	00098613          	mv	a2,s3
80000308:	00004097          	auipc	ra,0x4
8000030c:	990080e7          	jalr	-1648(ra) # 80003c98 <memmove>
80000310:	018a2c23          	sw	s8,24(s4)
80000314:	04ca0593          	addi	a1,s4,76
80000318:	050a0513          	addi	a0,s4,80
8000031c:	00098613          	mv	a2,s3
80000320:	00004097          	auipc	ra,0x4
80000324:	978080e7          	jalr	-1672(ra) # 80003c98 <memmove>
80000328:	049a2623          	sw	s1,76(s4)
8000032c:	028a1923          	sh	s0,50(s4)
80000330:	0d40006f          	j	80000404 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x3d4>
80000334:	ff9a0a13          	addi	s4,s4,-7
80000338:	01312e23          	sw	s3,28(sp)
8000033c:	03612023          	sw	s6,32(sp)
80000340:	02a12223          	sw	a0,36(sp)
80000344:	02810513          	addi	a0,sp,40
80000348:	01c10593          	addi	a1,sp,28
8000034c:	00000097          	auipc	ra,0x0
80000350:	304080e7          	jalr	772(ra) # 80000650 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E>
80000354:	03012b03          	lw	s6,48(sp)
80000358:	032b5b83          	lhu	s7,50(s6)
8000035c:	004b0513          	addi	a0,s6,4
80000360:	001a0d93          	addi	s11,s4,1
80000364:	002a1993          	slli	s3,s4,0x2
80000368:	01350433          	add	s0,a0,s3
8000036c:	002d9c93          	slli	s9,s11,0x2
80000370:	037a7e63          	bgeu	s4,s7,800003ac <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x37c>
80000374:	01950533          	add	a0,a0,s9
80000378:	414b8a33          	sub	s4,s7,s4
8000037c:	002a1a13          	slli	s4,s4,0x2
80000380:	00040593          	mv	a1,s0
80000384:	000a0613          	mv	a2,s4
80000388:	00004097          	auipc	ra,0x4
8000038c:	910080e7          	jalr	-1776(ra) # 80003c98 <memmove>
80000390:	034b0513          	addi	a0,s6,52
80000394:	019505b3          	add	a1,a0,s9
80000398:	01350533          	add	a0,a0,s3
8000039c:	00850513          	addi	a0,a0,8
800003a0:	000a0613          	mv	a2,s4
800003a4:	00004097          	auipc	ra,0x4
800003a8:	8f4080e7          	jalr	-1804(ra) # 80003c98 <memmove>
800003ac:	001b8513          	addi	a0,s7,1
800003b0:	01842023          	sw	s8,0(s0)
800003b4:	019b0cb3          	add	s9,s6,s9
800003b8:	002b8593          	addi	a1,s7,2
800003bc:	029caa23          	sw	s1,52(s9)
800003c0:	02ab1923          	sh	a0,50(s6)
800003c4:	e6bdfee3          	bgeu	s11,a1,80000240 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x210>
800003c8:	013b09b3          	add	s3,s6,s3
800003cc:	03898513          	addi	a0,s3,56
800003d0:	417005b3          	neg	a1,s7
800003d4:	00052603          	lw	a2,0(a0)
800003d8:	03b61823          	sh	s11,48(a2)
800003dc:	001d8d93          	addi	s11,s11,1
800003e0:	01662023          	sw	s6,0(a2)
800003e4:	01b58633          	add	a2,a1,s11
800003e8:	00450513          	addi	a0,a0,4
800003ec:	ff2614e3          	bne	a2,s2,800003d4 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x3a4>
800003f0:	e51ff06f          	j	80000240 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x210>
800003f4:	018a2c23          	sw	s8,24(s4)
800003f8:	049a2623          	sw	s1,76(s4)
800003fc:	028a1923          	sh	s0,50(s4)
80000400:	e5ab14e3          	bne	s6,s10,80000248 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x218>
80000404:	04ca0513          	addi	a0,s4,76
80000408:	416005b3          	neg	a1,s6
8000040c:	00600613          	li	a2,6
80000410:	00052683          	lw	a3,0(a0)
80000414:	02c69823          	sh	a2,48(a3)
80000418:	00160613          	addi	a2,a2,1
8000041c:	0146a023          	sw	s4,0(a3)
80000420:	00c586b3          	add	a3,a1,a2
80000424:	00450513          	addi	a0,a0,4
80000428:	ff2694e3          	bne	a3,s2,80000410 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x3e0>
8000042c:	e1dff06f          	j	80000248 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x218>
80000430:	00000d93          	li	s11,0
80000434:	00c12503          	lw	a0,12(sp)
80000438:	00052403          	lw	s0,0(a0)
8000043c:	00042903          	lw	s2,0(s0)
80000440:	1e090863          	beqz	s2,80000630 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x600>
80000444:	00442983          	lw	s3,4(s0)
80000448:	80005537          	lui	a0,0x80005
8000044c:	f2e54003          	lbu	zero,-210(a0) # 80004f2e <__rust_no_alloc_shim_is_unstable>
80000450:	80006537          	lui	a0,0x80006
80000454:	f3050513          	addi	a0,a0,-208 # 80005f30 <_ZN7SuperOS6kalloc16KERNEL_ALLOCATOR17h0e08997bf1bee0d5E.llvm.14510610307159664504>
80000458:	00400593          	li	a1,4
8000045c:	06400613          	li	a2,100
80000460:	00000097          	auipc	ra,0x0
80000464:	6c8080e7          	jalr	1736(ra) # 80000b28 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E>
80000468:	1c050c63          	beqz	a0,80000640 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x610>
8000046c:	00052023          	sw	zero,0(a0)
80000470:	02051923          	sh	zero,50(a0)
80000474:	03252a23          	sw	s2,52(a0)
80000478:	00198593          	addi	a1,s3,1
8000047c:	00a92023          	sw	a0,0(s2)
80000480:	02091823          	sh	zero,48(s2)
80000484:	00a42023          	sw	a0,0(s0)
80000488:	00b42223          	sw	a1,4(s0)
8000048c:	15b99e63          	bne	s3,s11,800005e8 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x5b8>
80000490:	03255583          	lhu	a1,50(a0)
80000494:	00b00613          	li	a2,11
80000498:	16c5f663          	bgeu	a1,a2,80000604 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x5d4>
8000049c:	03450613          	addi	a2,a0,52
800004a0:	00158693          	addi	a3,a1,1
800004a4:	02d51923          	sh	a3,50(a0)
800004a8:	00259593          	slli	a1,a1,0x2
800004ac:	00b505b3          	add	a1,a0,a1
800004b0:	0185a223          	sw	s8,4(a1)
800004b4:	00269593          	slli	a1,a3,0x2
800004b8:	00b605b3          	add	a1,a2,a1
800004bc:	0095a023          	sw	s1,0(a1)
800004c0:	00a4a023          	sw	a0,0(s1)
800004c4:	02d49823          	sh	a3,48(s1)
800004c8:	01412a03          	lw	s4,20(sp)
800004cc:	01812b83          	lw	s7,24(sp)
800004d0:	01012b03          	lw	s6,16(sp)
800004d4:	014ba023          	sw	s4,0(s7)
800004d8:	016ba223          	sw	s6,4(s7)
800004dc:	015ba423          	sw	s5,8(s7)
800004e0:	06c12083          	lw	ra,108(sp)
800004e4:	06812403          	lw	s0,104(sp)
800004e8:	06412483          	lw	s1,100(sp)
800004ec:	06012903          	lw	s2,96(sp)
800004f0:	05c12983          	lw	s3,92(sp)
800004f4:	05812a03          	lw	s4,88(sp)
800004f8:	05412a83          	lw	s5,84(sp)
800004fc:	05012b03          	lw	s6,80(sp)
80000500:	04c12b83          	lw	s7,76(sp)
80000504:	04812c03          	lw	s8,72(sp)
80000508:	04412c83          	lw	s9,68(sp)
8000050c:	04012d03          	lw	s10,64(sp)
80000510:	03c12d83          	lw	s11,60(sp)
80000514:	07010113          	addi	sp,sp,112
80000518:	00008067          	ret
8000051c:	001d8b93          	addi	s7,s11,1
80000520:	00498513          	addi	a0,s3,4
80000524:	001a0b13          	addi	s6,s4,1
80000528:	002a1413          	slli	s0,s4,0x2
8000052c:	00850933          	add	s2,a0,s0
80000530:	002b1c93          	slli	s9,s6,0x2
80000534:	03ba7e63          	bgeu	s4,s11,80000570 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x540>
80000538:	01950533          	add	a0,a0,s9
8000053c:	414d8a33          	sub	s4,s11,s4
80000540:	002a1a13          	slli	s4,s4,0x2
80000544:	00090593          	mv	a1,s2
80000548:	000a0613          	mv	a2,s4
8000054c:	00003097          	auipc	ra,0x3
80000550:	74c080e7          	jalr	1868(ra) # 80003c98 <memmove>
80000554:	03498513          	addi	a0,s3,52
80000558:	019505b3          	add	a1,a0,s9
8000055c:	00850533          	add	a0,a0,s0
80000560:	00850513          	addi	a0,a0,8
80000564:	000a0613          	mv	a2,s4
80000568:	00003097          	auipc	ra,0x3
8000056c:	730080e7          	jalr	1840(ra) # 80003c98 <memmove>
80000570:	01892023          	sw	s8,0(s2)
80000574:	01998cb3          	add	s9,s3,s9
80000578:	002d8513          	addi	a0,s11,2
8000057c:	029caa23          	sw	s1,52(s9)
80000580:	03799923          	sh	s7,50(s3)
80000584:	f4ab72e3          	bgeu	s6,a0,800004c8 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x498>
80000588:	00898433          	add	s0,s3,s0
8000058c:	03840513          	addi	a0,s0,56
80000590:	41b005b3          	neg	a1,s11
80000594:	00200613          	li	a2,2
80000598:	00052683          	lw	a3,0(a0)
8000059c:	03669823          	sh	s6,48(a3)
800005a0:	001b0b13          	addi	s6,s6,1
800005a4:	0136a023          	sw	s3,0(a3)
800005a8:	016586b3          	add	a3,a1,s6
800005ac:	00450513          	addi	a0,a0,4
800005b0:	fec694e3          	bne	a3,a2,80000598 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x568>
800005b4:	f15ff06f          	j	800004c8 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E+0x498>
800005b8:	80004537          	lui	a0,0x80004
800005bc:	1fc50513          	addi	a0,a0,508 # 800041fc <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.12>
800005c0:	80004637          	lui	a2,0x80004
800005c4:	23460613          	addi	a2,a2,564 # 80004234 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.13>
800005c8:	03500593          	li	a1,53
800005cc:	00002097          	auipc	ra,0x2
800005d0:	ae8080e7          	jalr	-1304(ra) # 800020b4 <_ZN4core9panicking5panic17h651cf8329c8a8911E>
800005d4:	80004637          	lui	a2,0x80004
800005d8:	1dc60613          	addi	a2,a2,476 # 800041dc <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.10>
800005dc:	00b00593          	li	a1,11
800005e0:	00003097          	auipc	ra,0x3
800005e4:	cb0080e7          	jalr	-848(ra) # 80003290 <_ZN4core5slice5index24slice_end_index_len_fail17h606d05af048992d1E>
800005e8:	80004537          	lui	a0,0x80004
800005ec:	15350513          	addi	a0,a0,339 # 80004153 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.5>
800005f0:	80004637          	lui	a2,0x80004
800005f4:	18460613          	addi	a2,a2,388 # 80004184 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.6>
800005f8:	03000593          	li	a1,48
800005fc:	00002097          	auipc	ra,0x2
80000600:	ab8080e7          	jalr	-1352(ra) # 800020b4 <_ZN4core9panicking5panic17h651cf8329c8a8911E>
80000604:	80004537          	lui	a0,0x80004
80000608:	0b450513          	addi	a0,a0,180 # 800040b4 <anon.0967ae7c4fd660b9acdb752d1aeda62f.2.llvm.1023813754811204676>
8000060c:	80004637          	lui	a2,0x80004
80000610:	19460613          	addi	a2,a2,404 # 80004194 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.7>
80000614:	02000593          	li	a1,32
80000618:	00002097          	auipc	ra,0x2
8000061c:	a9c080e7          	jalr	-1380(ra) # 800020b4 <_ZN4core9panicking5panic17h651cf8329c8a8911E>
80000620:	00400513          	li	a0,4
80000624:	03400593          	li	a1,52
80000628:	00002097          	auipc	ra,0x2
8000062c:	810080e7          	jalr	-2032(ra) # 80001e38 <_ZN5alloc5alloc18handle_alloc_error17hacdc36dbf7ea50caE>
80000630:	80004537          	lui	a0,0x80004
80000634:	08450513          	addi	a0,a0,132 # 80004084 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.1>
80000638:	00002097          	auipc	ra,0x2
8000063c:	8dc080e7          	jalr	-1828(ra) # 80001f14 <_ZN4core6option13unwrap_failed17ha917ca27cfe8d772E>
80000640:	00400513          	li	a0,4
80000644:	06400593          	li	a1,100
80000648:	00001097          	auipc	ra,0x1
8000064c:	7f0080e7          	jalr	2032(ra) # 80001e38 <_ZN5alloc5alloc18handle_alloc_error17hacdc36dbf7ea50caE>

80000650 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E>:
80000650:	fd010113          	addi	sp,sp,-48
80000654:	02112623          	sw	ra,44(sp)
80000658:	02812423          	sw	s0,40(sp)
8000065c:	02912223          	sw	s1,36(sp)
80000660:	03212023          	sw	s2,32(sp)
80000664:	01312e23          	sw	s3,28(sp)
80000668:	01412c23          	sw	s4,24(sp)
8000066c:	01512a23          	sw	s5,20(sp)
80000670:	01612823          	sw	s6,16(sp)
80000674:	01712623          	sw	s7,12(sp)
80000678:	01812423          	sw	s8,8(sp)
8000067c:	00058913          	mv	s2,a1
80000680:	0005aa03          	lw	s4,0(a1)
80000684:	032a5983          	lhu	s3,50(s4)
80000688:	800055b7          	lui	a1,0x80005
8000068c:	f2e5c003          	lbu	zero,-210(a1) # 80004f2e <__rust_no_alloc_shim_is_unstable>
80000690:	00050413          	mv	s0,a0
80000694:	80006537          	lui	a0,0x80006
80000698:	f3050513          	addi	a0,a0,-208 # 80005f30 <_ZN7SuperOS6kalloc16KERNEL_ALLOCATOR17h0e08997bf1bee0d5E.llvm.14510610307159664504>
8000069c:	00400593          	li	a1,4
800006a0:	06400613          	li	a2,100
800006a4:	00000097          	auipc	ra,0x0
800006a8:	484080e7          	jalr	1156(ra) # 80000b28 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E>
800006ac:	14050863          	beqz	a0,800007fc <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E+0x1ac>
800006b0:	00050493          	mv	s1,a0
800006b4:	00052023          	sw	zero,0(a0)
800006b8:	02051923          	sh	zero,50(a0)
800006bc:	00892b83          	lw	s7,8(s2)
800006c0:	032a5583          	lhu	a1,50(s4)
800006c4:	fffbc513          	not	a0,s7
800006c8:	00b50533          	add	a0,a0,a1
800006cc:	00c00613          	li	a2,12
800006d0:	02a49923          	sh	a0,50(s1)
800006d4:	10c57063          	bgeu	a0,a2,800007d4 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E+0x184>
800006d8:	001b8613          	addi	a2,s7,1
800006dc:	40c585b3          	sub	a1,a1,a2
800006e0:	0ca59c63          	bne	a1,a0,800007b8 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E+0x168>
800006e4:	004a0593          	addi	a1,s4,4
800006e8:	002b9c13          	slli	s8,s7,0x2
800006ec:	018586b3          	add	a3,a1,s8
800006f0:	0006aa83          	lw	s5,0(a3)
800006f4:	00448693          	addi	a3,s1,4
800006f8:	00261613          	slli	a2,a2,0x2
800006fc:	00c585b3          	add	a1,a1,a2
80000700:	00251613          	slli	a2,a0,0x2
80000704:	00068513          	mv	a0,a3
80000708:	00003097          	auipc	ra,0x3
8000070c:	480080e7          	jalr	1152(ra) # 80003b88 <memcpy>
80000710:	037a1923          	sh	s7,50(s4)
80000714:	0324db03          	lhu	s6,50(s1)
80000718:	00c00593          	li	a1,12
8000071c:	001b0513          	addi	a0,s6,1
80000720:	0cbb7463          	bgeu	s6,a1,800007e8 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E+0x198>
80000724:	41798633          	sub	a2,s3,s7
80000728:	08a61863          	bne	a2,a0,800007b8 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E+0x168>
8000072c:	018a0c33          	add	s8,s4,s8
80000730:	038c0593          	addi	a1,s8,56
80000734:	03448993          	addi	s3,s1,52
80000738:	00261613          	slli	a2,a2,0x2
8000073c:	00098513          	mv	a0,s3
80000740:	00003097          	auipc	ra,0x3
80000744:	448080e7          	jalr	1096(ra) # 80003b88 <memcpy>
80000748:	00492503          	lw	a0,4(s2)
8000074c:	00000593          	li	a1,0
80000750:	00259613          	slli	a2,a1,0x2
80000754:	00c98633          	add	a2,s3,a2
80000758:	00062603          	lw	a2,0(a2)
8000075c:	00962023          	sw	s1,0(a2)
80000760:	02b61823          	sh	a1,48(a2)
80000764:	0165f863          	bgeu	a1,s6,80000774 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E+0x124>
80000768:	0165b633          	sltu	a2,a1,s6
8000076c:	00c585b3          	add	a1,a1,a2
80000770:	febb70e3          	bgeu	s6,a1,80000750 <_ZN5alloc11collections5btree4node212Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Internal$GT$$C$alloc..collections..btree..node..marker..KV$GT$5split17hb12bc44e42d38af7E+0x100>
80000774:	01442023          	sw	s4,0(s0)
80000778:	00a42223          	sw	a0,4(s0)
8000077c:	01542823          	sw	s5,16(s0)
80000780:	00942423          	sw	s1,8(s0)
80000784:	00a42623          	sw	a0,12(s0)
80000788:	02c12083          	lw	ra,44(sp)
8000078c:	02812403          	lw	s0,40(sp)
80000790:	02412483          	lw	s1,36(sp)
80000794:	02012903          	lw	s2,32(sp)
80000798:	01c12983          	lw	s3,28(sp)
8000079c:	01812a03          	lw	s4,24(sp)
800007a0:	01412a83          	lw	s5,20(sp)
800007a4:	01012b03          	lw	s6,16(sp)
800007a8:	00c12b83          	lw	s7,12(sp)
800007ac:	00812c03          	lw	s8,8(sp)
800007b0:	03010113          	addi	sp,sp,48
800007b4:	00008067          	ret
800007b8:	80004537          	lui	a0,0x80004
800007bc:	1a450513          	addi	a0,a0,420 # 800041a4 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.8>
800007c0:	80004637          	lui	a2,0x80004
800007c4:	1cc60613          	addi	a2,a2,460 # 800041cc <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.9>
800007c8:	02800593          	li	a1,40
800007cc:	00002097          	auipc	ra,0x2
800007d0:	8e8080e7          	jalr	-1816(ra) # 800020b4 <_ZN4core9panicking5panic17h651cf8329c8a8911E>
800007d4:	80004637          	lui	a2,0x80004
800007d8:	1dc60613          	addi	a2,a2,476 # 800041dc <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.10>
800007dc:	00b00593          	li	a1,11
800007e0:	00003097          	auipc	ra,0x3
800007e4:	ab0080e7          	jalr	-1360(ra) # 80003290 <_ZN4core5slice5index24slice_end_index_len_fail17h606d05af048992d1E>
800007e8:	80004637          	lui	a2,0x80004
800007ec:	1ec60613          	addi	a2,a2,492 # 800041ec <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.11>
800007f0:	00c00593          	li	a1,12
800007f4:	00003097          	auipc	ra,0x3
800007f8:	a9c080e7          	jalr	-1380(ra) # 80003290 <_ZN4core5slice5index24slice_end_index_len_fail17h606d05af048992d1E>
800007fc:	00400513          	li	a0,4
80000800:	06400593          	li	a1,100
80000804:	00001097          	auipc	ra,0x1
80000808:	634080e7          	jalr	1588(ra) # 80001e38 <_ZN5alloc5alloc18handle_alloc_error17hacdc36dbf7ea50caE>

8000080c <_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h238390e4fa15678bE>:
8000080c:	00052503          	lw	a0,0(a0)
80000810:	00001317          	auipc	t1,0x1
80000814:	64c30067          	jr	1612(t1) # 80001e5c <_ZN68_$LT$core..ptr..alignment..Alignment$u20$as$u20$core..fmt..Debug$GT$3fmt17h68454b409a0fd924E>

80000818 <_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h02ba342068fcc2deE>:
80000818:	00052503          	lw	a0,0(a0)
8000081c:	00001317          	auipc	t1,0x1
80000820:	72030067          	jr	1824(t1) # 80001f3c <_ZN73_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$3fmt17h69b70629720e2a98E>

80000824 <_ZN54_$LT$$BP$const$u20$T$u20$as$u20$core..fmt..Pointer$GT$3fmt17hef7da24cb2e3fea3E>:
80000824:	00052503          	lw	a0,0(a0)
80000828:	00003317          	auipc	t1,0x3
8000082c:	98030067          	jr	-1664(t1) # 800031a8 <_ZN4core3fmt17pointer_fmt_inner17h3a78f71d335c4ae6E>

80000830 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE>:
80000830:	fc010113          	addi	sp,sp,-64
80000834:	02112e23          	sw	ra,60(sp)
80000838:	02812c23          	sw	s0,56(sp)
8000083c:	02912a23          	sw	s1,52(sp)
80000840:	00052603          	lw	a2,0(a0)
80000844:	00058413          	mv	s0,a1
80000848:	06060c63          	beqz	a2,800008c0 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0x90>
8000084c:	00452583          	lw	a1,4(a0)
80000850:	00100693          	li	a3,1
80000854:	03265783          	lhu	a5,50(a2)
80000858:	00460893          	addi	a7,a2,4
8000085c:	00279813          	slli	a6,a5,0x2
80000860:	fff00713          	li	a4,-1
80000864:	02080c63          	beqz	a6,8000089c <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0x6c>
80000868:	0008a283          	lw	t0,0(a7)
8000086c:	00488893          	addi	a7,a7,4
80000870:	00543333          	sltu	t1,s0,t0
80000874:	0082c2b3          	xor	t0,t0,s0
80000878:	005032b3          	snez	t0,t0
8000087c:	40600333          	neg	t1,t1
80000880:	005362b3          	or	t0,t1,t0
80000884:	ffc80813          	addi	a6,a6,-4
80000888:	00170713          	addi	a4,a4,1
8000088c:	fcd28ce3          	beq	t0,a3,80000864 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0x34>
80000890:	0ff2f793          	zext.b	a5,t0
80000894:	00079663          	bnez	a5,800008a0 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0x70>
80000898:	0200006f          	j	800008b8 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0x88>
8000089c:	00078713          	mv	a4,a5
800008a0:	06058e63          	beqz	a1,8000091c <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0xec>
800008a4:	00271713          	slli	a4,a4,0x2
800008a8:	00e60633          	add	a2,a2,a4
800008ac:	03462603          	lw	a2,52(a2)
800008b0:	fff58593          	addi	a1,a1,-1
800008b4:	fa1ff06f          	j	80000854 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0x24>
800008b8:	00100513          	li	a0,1
800008bc:	04c0006f          	j	80000908 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0xd8>
800008c0:	00050493          	mv	s1,a0
800008c4:	80005537          	lui	a0,0x80005
800008c8:	f2e54003          	lbu	zero,-210(a0) # 80004f2e <__rust_no_alloc_shim_is_unstable>
800008cc:	80006537          	lui	a0,0x80006
800008d0:	f3050513          	addi	a0,a0,-208 # 80005f30 <_ZN7SuperOS6kalloc16KERNEL_ALLOCATOR17h0e08997bf1bee0d5E.llvm.14510610307159664504>
800008d4:	00400593          	li	a1,4
800008d8:	03400613          	li	a2,52
800008dc:	00000097          	auipc	ra,0x0
800008e0:	24c080e7          	jalr	588(ra) # 80000b28 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E>
800008e4:	08050263          	beqz	a0,80000968 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0x138>
800008e8:	00052023          	sw	zero,0(a0)
800008ec:	00100593          	li	a1,1
800008f0:	02b51923          	sh	a1,50(a0)
800008f4:	00852223          	sw	s0,4(a0)
800008f8:	00a4a023          	sw	a0,0(s1)
800008fc:	0004a223          	sw	zero,4(s1)
80000900:	00b4a423          	sw	a1,8(s1)
80000904:	00000513          	li	a0,0
80000908:	03c12083          	lw	ra,60(sp)
8000090c:	03812403          	lw	s0,56(sp)
80000910:	03412483          	lw	s1,52(sp)
80000914:	04010113          	addi	sp,sp,64
80000918:	00008067          	ret
8000091c:	00a12423          	sw	a0,8(sp)
80000920:	00812623          	sw	s0,12(sp)
80000924:	00c12823          	sw	a2,16(sp)
80000928:	00012a23          	sw	zero,20(sp)
8000092c:	00e12c23          	sw	a4,24(sp)
80000930:	02e12223          	sw	a4,36(sp)
80000934:	02012023          	sw	zero,32(sp)
80000938:	00c12e23          	sw	a2,28(sp)
8000093c:	02810513          	addi	a0,sp,40
80000940:	01c10593          	addi	a1,sp,28
80000944:	00810693          	addi	a3,sp,8
80000948:	00040613          	mv	a2,s0
8000094c:	fffff097          	auipc	ra,0xfffff
80000950:	6e4080e7          	jalr	1764(ra) # 80000030 <_ZN5alloc11collections5btree4node210Handle$LT$alloc..collections..btree..node..NodeRef$LT$alloc..collections..btree..node..marker..Mut$C$K$C$V$C$alloc..collections..btree..node..marker..Leaf$GT$$C$alloc..collections..btree..node..marker..Edge$GT$16insert_recursing17h859d3e75df2c7a53E>
80000954:	00812503          	lw	a0,8(sp)
80000958:	00852583          	lw	a1,8(a0)
8000095c:	00158593          	addi	a1,a1,1
80000960:	00b52423          	sw	a1,8(a0)
80000964:	fa1ff06f          	j	80000904 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE+0xd4>
80000968:	00400513          	li	a0,4
8000096c:	03400593          	li	a1,52
80000970:	00001097          	auipc	ra,0x1
80000974:	4c8080e7          	jalr	1224(ra) # 80001e38 <_ZN5alloc5alloc18handle_alloc_error17hacdc36dbf7ea50caE>

80000978 <_ZN69_$LT$core..alloc..layout..LayoutError$u20$as$u20$core..fmt..Debug$GT$3fmt17hadc1b412746cacb9E>:
80000978:	800046b7          	lui	a3,0x80004
8000097c:	2d868693          	addi	a3,a3,728 # 800042d8 <.Lanon.272ad12a150edddff8aff02f5f98f349.2>
80000980:	00b00613          	li	a2,11
80000984:	00058513          	mv	a0,a1
80000988:	00068593          	mv	a1,a3
8000098c:	00002317          	auipc	t1,0x2
80000990:	6ac30067          	jr	1708(t1) # 80003038 <_ZN4core3fmt9Formatter9write_str17hd607abcbb12fb4c8E>

80000994 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E>:
80000994:	fe010113          	addi	sp,sp,-32
80000998:	00112e23          	sw	ra,28(sp)
8000099c:	00812c23          	sw	s0,24(sp)
800009a0:	00912a23          	sw	s1,20(sp)
800009a4:	01212823          	sw	s2,16(sp)
800009a8:	01312623          	sw	s3,12(sp)
800009ac:	01412423          	sw	s4,8(sp)
800009b0:	00052483          	lw	s1,0(a0)
800009b4:	12048263          	beqz	s1,80000ad8 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x144>
800009b8:	00852903          	lw	s2,8(a0)
800009bc:	00452983          	lw	s3,4(a0)
800009c0:	0c090263          	beqz	s2,80000a84 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0xf0>
800009c4:	00000593          	li	a1,0
800009c8:	80006437          	lui	s0,0x80006
800009cc:	f3040413          	addi	s0,s0,-208 # 80005f30 <_ZN7SuperOS6kalloc16KERNEL_ALLOCATOR17h0e08997bf1bee0d5E.llvm.14510610307159664504>
800009d0:	00048a13          	mv	s4,s1
800009d4:	0140006f          	j	800009e8 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x54>
800009d8:	fff90913          	addi	s2,s2,-1
800009dc:	00000a13          	li	s4,0
800009e0:	00048593          	mv	a1,s1
800009e4:	0a090863          	beqz	s2,80000a94 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x100>
800009e8:	00058863          	beqz	a1,800009f8 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x64>
800009ec:	0325d503          	lhu	a0,50(a1)
800009f0:	06a9fe63          	bgeu	s3,a0,80000a6c <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0xd8>
800009f4:	0240006f          	j	80000a18 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x84>
800009f8:	000a0593          	mv	a1,s4
800009fc:	00098863          	beqz	s3,80000a0c <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x78>
80000a00:	0345a583          	lw	a1,52(a1)
80000a04:	fff98993          	addi	s3,s3,-1
80000a08:	fe099ce3          	bnez	s3,80000a00 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x6c>
80000a0c:	00000a13          	li	s4,0
80000a10:	0325d503          	lhu	a0,50(a1)
80000a14:	04a9fc63          	bgeu	s3,a0,80000a6c <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0xd8>
80000a18:	00058493          	mv	s1,a1
80000a1c:	00198993          	addi	s3,s3,1
80000a20:	fa0a0ce3          	beqz	s4,800009d8 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x44>
80000a24:	00299993          	slli	s3,s3,0x2
80000a28:	013484b3          	add	s1,s1,s3
80000a2c:	03448513          	addi	a0,s1,52
80000a30:	00052483          	lw	s1,0(a0)
80000a34:	fffa0a13          	addi	s4,s4,-1
80000a38:	03448513          	addi	a0,s1,52
80000a3c:	fe0a1ae3          	bnez	s4,80000a30 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x9c>
80000a40:	00000993          	li	s3,0
80000a44:	f95ff06f          	j	800009d8 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x44>
80000a48:	0305d983          	lhu	s3,48(a1)
80000a4c:	001a0a13          	addi	s4,s4,1
80000a50:	00400613          	li	a2,4
80000a54:	00040513          	mv	a0,s0
80000a58:	00000097          	auipc	ra,0x0
80000a5c:	2c4080e7          	jalr	708(ra) # 80000d1c <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E>
80000a60:	0324d503          	lhu	a0,50(s1)
80000a64:	00048593          	mv	a1,s1
80000a68:	faa9eae3          	bltu	s3,a0,80000a1c <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x88>
80000a6c:	0005a483          	lw	s1,0(a1)
80000a70:	08048463          	beqz	s1,80000af8 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x164>
80000a74:	03400693          	li	a3,52
80000a78:	fc0a08e3          	beqz	s4,80000a48 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0xb4>
80000a7c:	06400693          	li	a3,100
80000a80:	fc9ff06f          	j	80000a48 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0xb4>
80000a84:	00098863          	beqz	s3,80000a94 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x100>
80000a88:	0344a483          	lw	s1,52(s1)
80000a8c:	fff98993          	addi	s3,s3,-1
80000a90:	fe099ce3          	bnez	s3,80000a88 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0xf4>
80000a94:	00000913          	li	s2,0
80000a98:	80006437          	lui	s0,0x80006
80000a9c:	f3040413          	addi	s0,s0,-208 # 80005f30 <_ZN7SuperOS6kalloc16KERNEL_ALLOCATOR17h0e08997bf1bee0d5E.llvm.14510610307159664504>
80000aa0:	0280006f          	j	80000ac8 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x134>
80000aa4:	0004a983          	lw	s3,0(s1)
80000aa8:	00400613          	li	a2,4
80000aac:	00040513          	mv	a0,s0
80000ab0:	00048593          	mv	a1,s1
80000ab4:	00000097          	auipc	ra,0x0
80000ab8:	268080e7          	jalr	616(ra) # 80000d1c <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E>
80000abc:	fff90913          	addi	s2,s2,-1
80000ac0:	00098493          	mv	s1,s3
80000ac4:	00098a63          	beqz	s3,80000ad8 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x144>
80000ac8:	03400693          	li	a3,52
80000acc:	fc090ce3          	beqz	s2,80000aa4 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x110>
80000ad0:	06400693          	li	a3,100
80000ad4:	fd1ff06f          	j	80000aa4 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x110>
80000ad8:	01c12083          	lw	ra,28(sp)
80000adc:	01812403          	lw	s0,24(sp)
80000ae0:	01412483          	lw	s1,20(sp)
80000ae4:	01012903          	lw	s2,16(sp)
80000ae8:	00c12983          	lw	s3,12(sp)
80000aec:	00812a03          	lw	s4,8(sp)
80000af0:	02010113          	addi	sp,sp,32
80000af4:	00008067          	ret
80000af8:	03400693          	li	a3,52
80000afc:	000a0463          	beqz	s4,80000b04 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E+0x170>
80000b00:	06400693          	li	a3,100
80000b04:	00400613          	li	a2,4
80000b08:	00058513          	mv	a0,a1
80000b0c:	00068593          	mv	a1,a3
80000b10:	00001097          	auipc	ra,0x1
80000b14:	294080e7          	jalr	660(ra) # 80001da4 <__rust_dealloc>
80000b18:	80004537          	lui	a0,0x80004
80000b1c:	2c850513          	addi	a0,a0,712 # 800042c8 <anon.0967ae7c4fd660b9acdb752d1aeda62f.15.llvm.1023813754811204676>
80000b20:	00001097          	auipc	ra,0x1
80000b24:	3f4080e7          	jalr	1012(ra) # 80001f14 <_ZN4core6option13unwrap_failed17ha917ca27cfe8d772E>

80000b28 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E>:
80000b28:	fd010113          	addi	sp,sp,-48
80000b2c:	02112623          	sw	ra,44(sp)
80000b30:	02812423          	sw	s0,40(sp)
80000b34:	02912223          	sw	s1,36(sp)
80000b38:	03212023          	sw	s2,32(sp)
80000b3c:	00800693          	li	a3,8
80000b40:	00058913          	mv	s2,a1
80000b44:	00050413          	mv	s0,a0
80000b48:	00c6e463          	bltu	a3,a2,80000b50 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x28>
80000b4c:	00800613          	li	a2,8
80000b50:	00360613          	addi	a2,a2,3
80000b54:	ffc67493          	andi	s1,a2,-4
80000b58:	00048513          	mv	a0,s1
80000b5c:	00090593          	mv	a1,s2
80000b60:	00003097          	auipc	ra,0x3
80000b64:	980080e7          	jalr	-1664(ra) # 800034e0 <_ZN4core5alloc6layout6Layout19is_size_align_valid17hfcf08246f9a22341E>
80000b68:	00410593          	addi	a1,sp,4
80000b6c:	10050a63          	beqz	a0,80000c80 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x158>
80000b70:	00842703          	lw	a4,8(s0)
80000b74:	10070663          	beqz	a4,80000c80 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x158>
80000b78:	00440693          	addi	a3,s0,4
80000b7c:	40a00533          	neg	a0,a0
80000b80:	01257633          	and	a2,a0,s2
80000b84:	00195513          	srli	a0,s2,0x1
80000b88:	555557b7          	lui	a5,0x55555
80000b8c:	55578793          	addi	a5,a5,1365 # 55555555 <.Lline_table_start2+0x555541b6>
80000b90:	00f57533          	and	a0,a0,a5
80000b94:	40a90533          	sub	a0,s2,a0
80000b98:	333337b7          	lui	a5,0x33333
80000b9c:	33378793          	addi	a5,a5,819 # 33333333 <.Lline_table_start2+0x33331f94>
80000ba0:	00f57833          	and	a6,a0,a5
80000ba4:	00255513          	srli	a0,a0,0x2
80000ba8:	00f57533          	and	a0,a0,a5
80000bac:	00a80533          	add	a0,a6,a0
80000bb0:	00455793          	srli	a5,a0,0x4
80000bb4:	00f50533          	add	a0,a0,a5
80000bb8:	0f0f17b7          	lui	a5,0xf0f1
80000bbc:	f0f78793          	addi	a5,a5,-241 # f0f0f0f <.Lline_table_start2+0xf0efb70>
80000bc0:	00f57533          	and	a0,a0,a5
80000bc4:	010107b7          	lui	a5,0x1010
80000bc8:	10178793          	addi	a5,a5,257 # 1010101 <.Lline_table_start2+0x100ed62>
80000bcc:	02f507b3          	mul	a5,a0,a5
80000bd0:	0187d793          	srli	a5,a5,0x18
80000bd4:	fff90813          	addi	a6,s2,-1
80000bd8:	412008b3          	neg	a7,s2
80000bdc:	00100293          	li	t0,1
80000be0:	0140006f          	j	80000bf4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0xcc>
80000be4:	00472503          	lw	a0,4(a4)
80000be8:	00070693          	mv	a3,a4
80000bec:	00050713          	mv	a4,a0
80000bf0:	08050863          	beqz	a0,80000c80 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x158>
80000bf4:	00072e03          	lw	t3,0(a4)
80000bf8:	fe9e66e3          	bltu	t3,s1,80000be4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0xbc>
80000bfc:	0e579463          	bne	a5,t0,80000ce4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x1bc>
80000c00:	00e80eb3          	add	t4,a6,a4
80000c04:	011ef3b3          	and	t2,t4,a7
80000c08:	00070513          	mv	a0,a4
80000c0c:	00e38863          	beq	t2,a4,80000c1c <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0xf4>
80000c10:	008e8e93          	addi	t4,t4,8
80000c14:	011ef533          	and	a0,t4,a7
80000c18:	40e50333          	sub	t1,a0,a4
80000c1c:	00950fb3          	add	t6,a0,s1
80000c20:	01c70f33          	add	t5,a4,t3
80000c24:	fdff60e3          	bltu	t5,t6,80000be4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0xbc>
80000c28:	41ff0eb3          	sub	t4,t5,t6
80000c2c:	020e8e63          	beqz	t4,80000c68 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x140>
80000c30:	003f8f93          	addi	t6,t6,3
80000c34:	ffcffe13          	andi	t3,t6,-4
80000c38:	008e0f93          	addi	t6,t3,8
80000c3c:	fbff64e3          	bltu	t5,t6,80000be4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0xbc>
80000c40:	0006a223          	sw	zero,4(a3)
80000c44:	00472783          	lw	a5,4(a4)
80000c48:	00072223          	sw	zero,4(a4)
80000c4c:	06e38e63          	beq	t2,a4,80000cc8 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x1a0>
80000c50:	01de2023          	sw	t4,0(t3)
80000c54:	00fe2223          	sw	a5,4(t3)
80000c58:	00672023          	sw	t1,0(a4)
80000c5c:	01c72223          	sw	t3,4(a4)
80000c60:	00e6a223          	sw	a4,4(a3)
80000c64:	0740006f          	j	80000cd8 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x1b0>
80000c68:	0006a223          	sw	zero,4(a3)
80000c6c:	00472783          	lw	a5,4(a4)
80000c70:	00072223          	sw	zero,4(a4)
80000c74:	04e39663          	bne	t2,a4,80000cc0 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x198>
80000c78:	00f6a223          	sw	a5,4(a3)
80000c7c:	04051e63          	bnez	a0,80000cd8 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x1b0>
80000c80:	00000493          	li	s1,0
80000c84:	0095a023          	sw	s1,0(a1)
80000c88:	00412583          	lw	a1,4(sp)
80000c8c:	00058c63          	beqz	a1,80000ca4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x17c>
80000c90:	00812583          	lw	a1,8(sp)
80000c94:	00042603          	lw	a2,0(s0)
80000c98:	00b605b3          	add	a1,a2,a1
80000c9c:	00b42023          	sw	a1,0(s0)
80000ca0:	0080006f          	j	80000ca8 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x180>
80000ca4:	00000513          	li	a0,0
80000ca8:	02c12083          	lw	ra,44(sp)
80000cac:	02812403          	lw	s0,40(sp)
80000cb0:	02412483          	lw	s1,36(sp)
80000cb4:	02012903          	lw	s2,32(sp)
80000cb8:	03010113          	addi	sp,sp,48
80000cbc:	00008067          	ret
80000cc0:	00070e13          	mv	t3,a4
80000cc4:	00030e93          	mv	t4,t1
80000cc8:	01de2023          	sw	t4,0(t3)
80000ccc:	00fe2223          	sw	a5,4(t3)
80000cd0:	01c6a223          	sw	t3,4(a3)
80000cd4:	fa0506e3          	beqz	a0,80000c80 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x158>
80000cd8:	00c12223          	sw	a2,4(sp)
80000cdc:	00810593          	addi	a1,sp,8
80000ce0:	fa5ff06f          	j	80000c84 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$5alloc17hfa56188228b57e71E+0x15c>
80000ce4:	80004537          	lui	a0,0x80004
80000ce8:	31850513          	addi	a0,a0,792 # 80004318 <.Lanon.272ad12a150edddff8aff02f5f98f349.5>
80000cec:	00a12423          	sw	a0,8(sp)
80000cf0:	00100513          	li	a0,1
80000cf4:	00a12623          	sw	a0,12(sp)
80000cf8:	00012c23          	sw	zero,24(sp)
80000cfc:	00400513          	li	a0,4
80000d00:	00a12823          	sw	a0,16(sp)
80000d04:	00012a23          	sw	zero,20(sp)
80000d08:	800045b7          	lui	a1,0x80004
80000d0c:	39458593          	addi	a1,a1,916 # 80004394 <.Lanon.272ad12a150edddff8aff02f5f98f349.7>
80000d10:	00810513          	addi	a0,sp,8
80000d14:	00001097          	auipc	ra,0x1
80000d18:	374080e7          	jalr	884(ra) # 80002088 <_ZN4core9panicking9panic_fmt17hd44f1c16c40b716eE>

80000d1c <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E>:
80000d1c:	fb010113          	addi	sp,sp,-80
80000d20:	04112623          	sw	ra,76(sp)
80000d24:	04812423          	sw	s0,72(sp)
80000d28:	04912223          	sw	s1,68(sp)
80000d2c:	05212023          	sw	s2,64(sp)
80000d30:	00800713          	li	a4,8
80000d34:	00058913          	mv	s2,a1
80000d38:	00050413          	mv	s0,a0
80000d3c:	00d76463          	bltu	a4,a3,80000d44 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x28>
80000d40:	00800693          	li	a3,8
80000d44:	00368693          	addi	a3,a3,3
80000d48:	ffc6f493          	andi	s1,a3,-4
80000d4c:	00048513          	mv	a0,s1
80000d50:	00060593          	mv	a1,a2
80000d54:	00002097          	auipc	ra,0x2
80000d58:	78c080e7          	jalr	1932(ra) # 800034e0 <_ZN4core5alloc6layout6Layout19is_size_align_valid17hfcf08246f9a22341E>
80000d5c:	26050663          	beqz	a0,80000fc8 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x2ac>
80000d60:	00992023          	sw	s1,0(s2)
80000d64:	00092223          	sw	zero,4(s2)
80000d68:	00842583          	lw	a1,8(s0)
80000d6c:	04058263          	beqz	a1,80000db0 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x94>
80000d70:	01042503          	lw	a0,16(s0)
80000d74:	04b97e63          	bgeu	s2,a1,80000dd0 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0xb4>
80000d78:	009906b3          	add	a3,s2,s1
80000d7c:	16d5e663          	bltu	a1,a3,80000ee8 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x1cc>
80000d80:	00c42603          	lw	a2,12(s0)
80000d84:	00860793          	addi	a5,a2,8
80000d88:	00090713          	mv	a4,s2
80000d8c:	00f97a63          	bgeu	s2,a5,80000da0 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x84>
80000d90:	40c686b3          	sub	a3,a3,a2
80000d94:	00d62023          	sw	a3,0(a2)
80000d98:	00062223          	sw	zero,4(a2)
80000d9c:	00060713          	mv	a4,a2
80000da0:	00e42423          	sw	a4,8(s0)
80000da4:	00b72223          	sw	a1,4(a4)
80000da8:	00100613          	li	a2,1
80000dac:	09c0006f          	j	80000e48 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x12c>
80000db0:	00c42503          	lw	a0,12(s0)
80000db4:	00850593          	addi	a1,a0,8
80000db8:	0cb97c63          	bgeu	s2,a1,80000e90 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x174>
80000dbc:	012485b3          	add	a1,s1,s2
80000dc0:	40a585b3          	sub	a1,a1,a0
80000dc4:	00b52023          	sw	a1,0(a0)
80000dc8:	00052223          	sw	zero,4(a0)
80000dcc:	0cc0006f          	j	80000e98 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x17c>
80000dd0:	0045a603          	lw	a2,4(a1)
80000dd4:	01212223          	sw	s2,4(sp)
80000dd8:	02060263          	beqz	a2,80000dfc <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0xe0>
80000ddc:	00c96c63          	bltu	s2,a2,80000df4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0xd8>
80000de0:	00060593          	mv	a1,a2
80000de4:	00462603          	lw	a2,4(a2)
80000de8:	01212223          	sw	s2,4(sp)
80000dec:	00060863          	beqz	a2,80000dfc <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0xe0>
80000df0:	fec978e3          	bgeu	s2,a2,80000de0 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0xc4>
80000df4:	009906b3          	add	a3,s2,s1
80000df8:	18d66c63          	bltu	a2,a3,80000f90 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x274>
80000dfc:	00b12423          	sw	a1,8(sp)
80000e00:	0005a683          	lw	a3,0(a1)
80000e04:	00d58733          	add	a4,a1,a3
80000e08:	00d12623          	sw	a3,12(sp)
80000e0c:	10e96a63          	bltu	s2,a4,80000f20 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x204>
80000e10:	0125a223          	sw	s2,4(a1)
80000e14:	00c92223          	sw	a2,4(s2)
80000e18:	00200613          	li	a2,2
80000e1c:	00058913          	mv	s2,a1
80000e20:	0280006f          	j	80000e48 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x12c>
80000e24:	00072583          	lw	a1,0(a4)
80000e28:	00472683          	lw	a3,4(a4)
80000e2c:	00072223          	sw	zero,4(a4)
80000e30:	00092703          	lw	a4,0(s2)
80000e34:	00d92223          	sw	a3,4(s2)
80000e38:	00b705b3          	add	a1,a4,a1
80000e3c:	00b92023          	sw	a1,0(s2)
80000e40:	fff60613          	addi	a2,a2,-1
80000e44:	08060063          	beqz	a2,80000ec4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x1a8>
80000e48:	00492703          	lw	a4,4(s2)
80000e4c:	00092683          	lw	a3,0(s2)
80000e50:	00d905b3          	add	a1,s2,a3
80000e54:	00070c63          	beqz	a4,80000e6c <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x150>
80000e58:	fce586e3          	beq	a1,a4,80000e24 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x108>
80000e5c:	00070913          	mv	s2,a4
80000e60:	fff60613          	addi	a2,a2,-1
80000e64:	fe0612e3          	bnez	a2,80000e48 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x12c>
80000e68:	05c0006f          	j	80000ec4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x1a8>
80000e6c:	04a5fc63          	bgeu	a1,a0,80000ec4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x1a8>
80000e70:	00358613          	addi	a2,a1,3
80000e74:	ffc67613          	andi	a2,a2,-4
80000e78:	00860613          	addi	a2,a2,8
80000e7c:	04c57463          	bgeu	a0,a2,80000ec4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x1a8>
80000e80:	00a68533          	add	a0,a3,a0
80000e84:	40b50533          	sub	a0,a0,a1
80000e88:	00a92023          	sw	a0,0(s2)
80000e8c:	0380006f          	j	80000ec4 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x1a8>
80000e90:	00048593          	mv	a1,s1
80000e94:	00090513          	mv	a0,s2
80000e98:	01042603          	lw	a2,16(s0)
80000e9c:	00b506b3          	add	a3,a0,a1
80000ea0:	02c6f063          	bgeu	a3,a2,80000ec0 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x1a4>
80000ea4:	00368713          	addi	a4,a3,3
80000ea8:	ffc77713          	andi	a4,a4,-4
80000eac:	00870713          	addi	a4,a4,8
80000eb0:	00e67863          	bgeu	a2,a4,80000ec0 <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E+0x1a4>
80000eb4:	40d60633          	sub	a2,a2,a3
80000eb8:	00b605b3          	add	a1,a2,a1
80000ebc:	00b52023          	sw	a1,0(a0)
80000ec0:	00a42423          	sw	a0,8(s0)
80000ec4:	00042503          	lw	a0,0(s0)
80000ec8:	40950533          	sub	a0,a0,s1
80000ecc:	00a42023          	sw	a0,0(s0)
80000ed0:	04c12083          	lw	ra,76(sp)
80000ed4:	04812403          	lw	s0,72(sp)
80000ed8:	04412483          	lw	s1,68(sp)
80000edc:	04012903          	lw	s2,64(sp)
80000ee0:	05010113          	addi	sp,sp,80
80000ee4:	00008067          	ret
80000ee8:	80004537          	lui	a0,0x80004
80000eec:	41c50513          	addi	a0,a0,1052 # 8000441c <.Lanon.272ad12a150edddff8aff02f5f98f349.16>
80000ef0:	02a12423          	sw	a0,40(sp)
80000ef4:	00100513          	li	a0,1
80000ef8:	02a12623          	sw	a0,44(sp)
80000efc:	02012c23          	sw	zero,56(sp)
80000f00:	00400513          	li	a0,4
80000f04:	02a12823          	sw	a0,48(sp)
80000f08:	02012a23          	sw	zero,52(sp)
80000f0c:	800045b7          	lui	a1,0x80004
80000f10:	42458593          	addi	a1,a1,1060 # 80004424 <.Lanon.272ad12a150edddff8aff02f5f98f349.17>
80000f14:	02810513          	addi	a0,sp,40
80000f18:	00001097          	auipc	ra,0x1
80000f1c:	170080e7          	jalr	368(ra) # 80002088 <_ZN4core9panicking9panic_fmt17hd44f1c16c40b716eE>
80000f20:	00410513          	addi	a0,sp,4
80000f24:	02a12423          	sw	a0,40(sp)
80000f28:	80001537          	lui	a0,0x80001
80000f2c:	82450513          	addi	a0,a0,-2012 # 80000824 <_ZN54_$LT$$BP$const$u20$T$u20$as$u20$core..fmt..Pointer$GT$3fmt17hef7da24cb2e3fea3E>
80000f30:	02a12623          	sw	a0,44(sp)
80000f34:	00810593          	addi	a1,sp,8
80000f38:	02b12823          	sw	a1,48(sp)
80000f3c:	02a12a23          	sw	a0,52(sp)
80000f40:	00c10513          	addi	a0,sp,12
80000f44:	02a12c23          	sw	a0,56(sp)
80000f48:	80004537          	lui	a0,0x80004
80000f4c:	82050513          	addi	a0,a0,-2016 # 80003820 <_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17he9e9e363faaccf29E>
80000f50:	02a12e23          	sw	a0,60(sp)
80000f54:	80004537          	lui	a0,0x80004
80000f58:	47850513          	addi	a0,a0,1144 # 80004478 <.Lanon.272ad12a150edddff8aff02f5f98f349.23>
80000f5c:	00a12823          	sw	a0,16(sp)
80000f60:	00400513          	li	a0,4
80000f64:	00a12a23          	sw	a0,20(sp)
80000f68:	02012023          	sw	zero,32(sp)
80000f6c:	02810513          	addi	a0,sp,40
80000f70:	00a12c23          	sw	a0,24(sp)
80000f74:	00300513          	li	a0,3
80000f78:	00a12e23          	sw	a0,28(sp)
80000f7c:	800045b7          	lui	a1,0x80004
80000f80:	49858593          	addi	a1,a1,1176 # 80004498 <.Lanon.272ad12a150edddff8aff02f5f98f349.24>
80000f84:	01010513          	addi	a0,sp,16
80000f88:	00001097          	auipc	ra,0x1
80000f8c:	100080e7          	jalr	256(ra) # 80002088 <_ZN4core9panicking9panic_fmt17hd44f1c16c40b716eE>
80000f90:	80004537          	lui	a0,0x80004
80000f94:	41c50513          	addi	a0,a0,1052 # 8000441c <.Lanon.272ad12a150edddff8aff02f5f98f349.16>
80000f98:	02a12423          	sw	a0,40(sp)
80000f9c:	00100513          	li	a0,1
80000fa0:	02a12623          	sw	a0,44(sp)
80000fa4:	02012c23          	sw	zero,56(sp)
80000fa8:	00400513          	li	a0,4
80000fac:	02a12823          	sw	a0,48(sp)
80000fb0:	02012a23          	sw	zero,52(sp)
80000fb4:	800045b7          	lui	a1,0x80004
80000fb8:	43458593          	addi	a1,a1,1076 # 80004434 <.Lanon.272ad12a150edddff8aff02f5f98f349.18>
80000fbc:	02810513          	addi	a0,sp,40
80000fc0:	00001097          	auipc	ra,0x1
80000fc4:	0c8080e7          	jalr	200(ra) # 80002088 <_ZN4core9panicking9panic_fmt17hd44f1c16c40b716eE>
80000fc8:	80004537          	lui	a0,0x80004
80000fcc:	3b450513          	addi	a0,a0,948 # 800043b4 <.Lanon.272ad12a150edddff8aff02f5f98f349.13>
80000fd0:	800046b7          	lui	a3,0x80004
80000fd4:	3a468693          	addi	a3,a3,932 # 800043a4 <.Lanon.272ad12a150edddff8aff02f5f98f349.12>
80000fd8:	80004737          	lui	a4,0x80004
80000fdc:	3e070713          	addi	a4,a4,992 # 800043e0 <.Lanon.272ad12a150edddff8aff02f5f98f349.14>
80000fe0:	02b00593          	li	a1,43
80000fe4:	02810613          	addi	a2,sp,40
80000fe8:	00001097          	auipc	ra,0x1
80000fec:	114080e7          	jalr	276(ra) # 800020fc <_ZN4core6result13unwrap_failed17h987d8f67a7161eb1E>

80000ff0 <_ZN7SuperOS6palloc4init17hb4eb3dcde82ecf87E>:
80000ff0:	f7010113          	addi	sp,sp,-144
80000ff4:	08112623          	sw	ra,140(sp)
80000ff8:	08812423          	sw	s0,136(sp)
80000ffc:	08912223          	sw	s1,132(sp)
80001000:	09212023          	sw	s2,128(sp)
80001004:	07312e23          	sw	s3,124(sp)
80001008:	80006937          	lui	s2,0x80006
8000100c:	00090913          	mv	s2,s2
80001010:	03212023          	sw	s2,32(sp)
80001014:	02010513          	addi	a0,sp,32
80001018:	00a12423          	sw	a0,8(sp)
8000101c:	800039b7          	lui	s3,0x80003
80001020:	62098993          	addi	s3,s3,1568 # 80003620 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hcc6fe785c9e9110eE>
80001024:	01312623          	sw	s3,12(sp)
80001028:	80004537          	lui	a0,0x80004
8000102c:	50850513          	addi	a0,a0,1288 # 80004508 <.Lanon.272ad12a150edddff8aff02f5f98f349.32>
80001030:	02a12c23          	sw	a0,56(sp)
80001034:	00200513          	li	a0,2
80001038:	02a12e23          	sw	a0,60(sp)
8000103c:	04012423          	sw	zero,72(sp)
80001040:	00810513          	addi	a0,sp,8
80001044:	04a12023          	sw	a0,64(sp)
80001048:	00100513          	li	a0,1
8000104c:	04a12223          	sw	a0,68(sp)
80001050:	800045b7          	lui	a1,0x80004
80001054:	64458593          	addi	a1,a1,1604 # 80004644 <anon.58335f229ba488831bc287488e11d397.25.llvm.14510610307159664504>
80001058:	07b10513          	addi	a0,sp,123
8000105c:	03810613          	addi	a2,sp,56
80001060:	00001097          	auipc	ra,0x1
80001064:	6dc080e7          	jalr	1756(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
80001068:	16051663          	bnez	a0,800011d4 <_ZN7SuperOS6palloc4init17hb4eb3dcde82ecf87E+0x1e4>
8000106c:	80005537          	lui	a0,0x80005
80001070:	f2c54583          	lbu	a1,-212(a0) # 80004f2c <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.3.llvm.10321614113892589689>
80001074:	00058463          	beqz	a1,8000107c <_ZN7SuperOS6palloc4init17hb4eb3dcde82ecf87E+0x8c>
80001078:	0000006f          	j	80001078 <_ZN7SuperOS6palloc4init17hb4eb3dcde82ecf87E+0x88>
8000107c:	00100593          	li	a1,1
80001080:	f2b50623          	sb	a1,-212(a0)
80001084:	01e00537          	lui	a0,0x1e00
80001088:	00a90933          	add	s2,s2,a0
8000108c:	00c95493          	srli	s1,s2,0xc
80001090:	00148413          	addi	s0,s1,1
80001094:	02812823          	sw	s0,48(sp)
80001098:	00082537          	lui	a0,0x82
8000109c:	fff50513          	addi	a0,a0,-1 # 81fff <.Lline_table_start2+0x80c60>
800010a0:	02a12a23          	sw	a0,52(sp)
800010a4:	03010513          	addi	a0,sp,48
800010a8:	02a12023          	sw	a0,32(sp)
800010ac:	03312223          	sw	s3,36(sp)
800010b0:	03410513          	addi	a0,sp,52
800010b4:	02a12423          	sw	a0,40(sp)
800010b8:	03312623          	sw	s3,44(sp)
800010bc:	00200513          	li	a0,2
800010c0:	02a12c23          	sw	a0,56(sp)
800010c4:	04a12023          	sw	a0,64(sp)
800010c8:	02000613          	li	a2,32
800010cc:	04c12423          	sw	a2,72(sp)
800010d0:	04012623          	sw	zero,76(sp)
800010d4:	00400693          	li	a3,4
800010d8:	04d12823          	sw	a3,80(sp)
800010dc:	00300713          	li	a4,3
800010e0:	04e10a23          	sb	a4,84(sp)
800010e4:	04a12c23          	sw	a0,88(sp)
800010e8:	06a12023          	sw	a0,96(sp)
800010ec:	06c12423          	sw	a2,104(sp)
800010f0:	06b12623          	sw	a1,108(sp)
800010f4:	06d12823          	sw	a3,112(sp)
800010f8:	06e10a23          	sb	a4,116(sp)
800010fc:	800045b7          	lui	a1,0x80004
80001100:	4c058593          	addi	a1,a1,1216 # 800044c0 <.Lanon.272ad12a150edddff8aff02f5f98f349.28>
80001104:	00b12423          	sw	a1,8(sp)
80001108:	00e12623          	sw	a4,12(sp)
8000110c:	03810593          	addi	a1,sp,56
80001110:	00b12c23          	sw	a1,24(sp)
80001114:	00a12e23          	sw	a0,28(sp)
80001118:	02010593          	addi	a1,sp,32
8000111c:	00b12823          	sw	a1,16(sp)
80001120:	00a12a23          	sw	a0,20(sp)
80001124:	800045b7          	lui	a1,0x80004
80001128:	64458593          	addi	a1,a1,1604 # 80004644 <anon.58335f229ba488831bc287488e11d397.25.llvm.14510610307159664504>
8000112c:	07b10513          	addi	a0,sp,123
80001130:	00810613          	addi	a2,sp,8
80001134:	00001097          	auipc	ra,0x1
80001138:	608080e7          	jalr	1544(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
8000113c:	08051c63          	bnez	a0,800011d4 <_ZN7SuperOS6palloc4init17hb4eb3dcde82ecf87E+0x1e4>
80001140:	81ffe537          	lui	a0,0x81ffe
80001144:	fff50513          	addi	a0,a0,-1 # 81ffdfff <KALLOC_BUFFER+0x1ff7fff>
80001148:	07256463          	bltu	a0,s2,800011b0 <_ZN7SuperOS6palloc4init17hb4eb3dcde82ecf87E+0x1c0>
8000114c:	80005537          	lui	a0,0x80005
80001150:	f2052603          	lw	a2,-224(a0) # 80004f20 <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.0.llvm.10321614113892589689>
80001154:	800055b7          	lui	a1,0x80005
80001158:	f245a683          	lw	a3,-220(a1) # 80004f24 <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.1.llvm.10321614113892589689>
8000115c:	00c41713          	slli	a4,s0,0xc
80001160:	00c72023          	sw	a2,0(a4)
80001164:	00d72223          	sw	a3,4(a4)
80001168:	00100613          	li	a2,1
8000116c:	f2c52023          	sw	a2,-224(a0)
80001170:	000826b7          	lui	a3,0x82
80001174:	ffd68693          	addi	a3,a3,-3 # 81ffd <.Lline_table_start2+0x80c5e>
80001178:	f285a223          	sw	s0,-220(a1)
8000117c:	02d48a63          	beq	s1,a3,800011b0 <_ZN7SuperOS6palloc4init17hb4eb3dcde82ecf87E+0x1c0>
80001180:	00c49713          	slli	a4,s1,0xc
80001184:	000027b7          	lui	a5,0x2
80001188:	00f70733          	add	a4,a4,a5
8000118c:	000017b7          	lui	a5,0x1
80001190:	00040813          	mv	a6,s0
80001194:	00c72023          	sw	a2,0(a4)
80001198:	00872223          	sw	s0,4(a4)
8000119c:	f2c52023          	sw	a2,-224(a0)
800011a0:	00140413          	addi	s0,s0,1
800011a4:	f285a223          	sw	s0,-220(a1)
800011a8:	00f70733          	add	a4,a4,a5
800011ac:	fed812e3          	bne	a6,a3,80001190 <_ZN7SuperOS6palloc4init17hb4eb3dcde82ecf87E+0x1a0>
800011b0:	80005537          	lui	a0,0x80005
800011b4:	f2050623          	sb	zero,-212(a0) # 80004f2c <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.3.llvm.10321614113892589689>
800011b8:	08c12083          	lw	ra,140(sp)
800011bc:	08812403          	lw	s0,136(sp)
800011c0:	08412483          	lw	s1,132(sp)
800011c4:	08012903          	lw	s2,128(sp)
800011c8:	07c12983          	lw	s3,124(sp)
800011cc:	09010113          	addi	sp,sp,144
800011d0:	00008067          	ret
800011d4:	80004537          	lui	a0,0x80004
800011d8:	66c50513          	addi	a0,a0,1644 # 8000466c <anon.58335f229ba488831bc287488e11d397.27.llvm.14510610307159664504>
800011dc:	800046b7          	lui	a3,0x80004
800011e0:	65c68693          	addi	a3,a3,1628 # 8000465c <anon.58335f229ba488831bc287488e11d397.26.llvm.14510610307159664504>
800011e4:	80004737          	lui	a4,0x80004
800011e8:	6a870713          	addi	a4,a4,1704 # 800046a8 <anon.58335f229ba488831bc287488e11d397.29.llvm.14510610307159664504>
800011ec:	02b00593          	li	a1,43
800011f0:	07b10613          	addi	a2,sp,123
800011f4:	00001097          	auipc	ra,0x1
800011f8:	f08080e7          	jalr	-248(ra) # 800020fc <_ZN4core6result13unwrap_failed17h987d8f67a7161eb1E>
800011fc:	0000                	.insn	2, 0x
	...

80001200 <run_user>:
80001200:	fc410113          	addi	sp,sp,-60
80001204:	00312023          	sw	gp,0(sp)
80001208:	00412223          	sw	tp,4(sp)
8000120c:	00112423          	sw	ra,8(sp)
80001210:	00812623          	sw	s0,12(sp)
80001214:	00912823          	sw	s1,16(sp)
80001218:	01212a23          	sw	s2,20(sp)
8000121c:	01312c23          	sw	s3,24(sp)
80001220:	01412e23          	sw	s4,28(sp)
80001224:	03512023          	sw	s5,32(sp)
80001228:	03612223          	sw	s6,36(sp)
8000122c:	03712423          	sw	s7,40(sp)
80001230:	03812623          	sw	s8,44(sp)
80001234:	03912823          	sw	s9,48(sp)
80001238:	03a12a23          	sw	s10,52(sp)
8000123c:	03b12c23          	sw	s11,56(sp)
80001240:	34051073          	.insn	4, 0x34051073
80001244:	08252023          	sw	sp,128(a0)
80001248:	07c52283          	lw	t0,124(a0)
8000124c:	34129073          	.insn	4, 0x34129073
80001250:	00052083          	lw	ra,0(a0)
80001254:	00452103          	lw	sp,4(a0)
80001258:	00852183          	lw	gp,8(a0)
8000125c:	00c52203          	lw	tp,12(a0)
80001260:	01052283          	lw	t0,16(a0)
80001264:	01452303          	lw	t1,20(a0)
80001268:	01852383          	lw	t2,24(a0)
8000126c:	01c52403          	lw	s0,28(a0)
80001270:	02052483          	lw	s1,32(a0)
80001274:	02852583          	lw	a1,40(a0)
80001278:	02c52603          	lw	a2,44(a0)
8000127c:	03052683          	lw	a3,48(a0)
80001280:	03452703          	lw	a4,52(a0)
80001284:	03852783          	lw	a5,56(a0)
80001288:	03c52803          	lw	a6,60(a0)
8000128c:	04052883          	lw	a7,64(a0)
80001290:	04452903          	lw	s2,68(a0)
80001294:	04852983          	lw	s3,72(a0)
80001298:	04c52a03          	lw	s4,76(a0)
8000129c:	05052a83          	lw	s5,80(a0)
800012a0:	05452b03          	lw	s6,84(a0)
800012a4:	05852b83          	lw	s7,88(a0)
800012a8:	05c52c03          	lw	s8,92(a0)
800012ac:	06052c83          	lw	s9,96(a0)
800012b0:	06452d03          	lw	s10,100(a0)
800012b4:	06852d83          	lw	s11,104(a0)
800012b8:	06c52e03          	lw	t3,108(a0)
800012bc:	07052e83          	lw	t4,112(a0)
800012c0:	07452f03          	lw	t5,116(a0)
800012c4:	07852f83          	lw	t6,120(a0)
800012c8:	02452503          	lw	a0,36(a0)
800012cc:	30200073          	mret

800012d0 <user_trap>:
800012d0:	34051573          	.insn	4, 0x34051573
800012d4:	00152023          	sw	ra,0(a0)
800012d8:	00252223          	sw	sp,4(a0)
800012dc:	00352423          	sw	gp,8(a0)
800012e0:	00452623          	sw	tp,12(a0)
800012e4:	00552823          	sw	t0,16(a0)
800012e8:	00652a23          	sw	t1,20(a0)
800012ec:	00752c23          	sw	t2,24(a0)
800012f0:	00852e23          	sw	s0,28(a0)
800012f4:	02952023          	sw	s1,32(a0)
800012f8:	02b52423          	sw	a1,40(a0)
800012fc:	02c52623          	sw	a2,44(a0)
80001300:	02d52823          	sw	a3,48(a0)
80001304:	02e52a23          	sw	a4,52(a0)
80001308:	02f52c23          	sw	a5,56(a0)
8000130c:	03052e23          	sw	a6,60(a0)
80001310:	05152023          	sw	a7,64(a0)
80001314:	05252223          	sw	s2,68(a0)
80001318:	05352423          	sw	s3,72(a0)
8000131c:	05452623          	sw	s4,76(a0)
80001320:	05552823          	sw	s5,80(a0)
80001324:	05652a23          	sw	s6,84(a0)
80001328:	05752c23          	sw	s7,88(a0)
8000132c:	05852e23          	sw	s8,92(a0)
80001330:	07952023          	sw	s9,96(a0)
80001334:	07a52223          	sw	s10,100(a0)
80001338:	07b52423          	sw	s11,104(a0)
8000133c:	07c52623          	sw	t3,108(a0)
80001340:	07d52823          	sw	t4,112(a0)
80001344:	07e52a23          	sw	t5,116(a0)
80001348:	07f52c23          	sw	t6,120(a0)
8000134c:	340022f3          	.insn	4, 0x340022f3
80001350:	02552223          	sw	t0,36(a0)
80001354:	08052103          	lw	sp,128(a0)
80001358:	341022f3          	.insn	4, 0x341022f3
8000135c:	06552e23          	sw	t0,124(a0)
80001360:	00012183          	lw	gp,0(sp)
80001364:	00412203          	lw	tp,4(sp)
80001368:	00812083          	lw	ra,8(sp)
8000136c:	00c12403          	lw	s0,12(sp)
80001370:	01012483          	lw	s1,16(sp)
80001374:	01412903          	lw	s2,20(sp)
80001378:	01812983          	lw	s3,24(sp)
8000137c:	01c12a03          	lw	s4,28(sp)
80001380:	02012a83          	lw	s5,32(sp)
80001384:	02412b03          	lw	s6,36(sp)
80001388:	02812b83          	lw	s7,40(sp)
8000138c:	02c12c03          	lw	s8,44(sp)
80001390:	03012c83          	lw	s9,48(sp)
80001394:	03412d03          	lw	s10,52(sp)
80001398:	03812d83          	lw	s11,56(sp)
8000139c:	03c10113          	addi	sp,sp,60
800013a0:	00008067          	ret

800013a4 <_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..Debug$u20$for$u20$usize$GT$3fmt17h10cd62dad8640825E>:
800013a4:	01c5a603          	lw	a2,28(a1)
800013a8:	01067693          	andi	a3,a2,16
800013ac:	00069a63          	bnez	a3,800013c0 <_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..Debug$u20$for$u20$usize$GT$3fmt17h10cd62dad8640825E+0x1c>
800013b0:	02067613          	andi	a2,a2,32
800013b4:	00061a63          	bnez	a2,800013c8 <_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..Debug$u20$for$u20$usize$GT$3fmt17h10cd62dad8640825E+0x24>
800013b8:	00002317          	auipc	t1,0x2
800013bc:	46830067          	jr	1128(t1) # 80003820 <_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17he9e9e363faaccf29E>
800013c0:	00002317          	auipc	t1,0x2
800013c4:	26030067          	jr	608(t1) # 80003620 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hcc6fe785c9e9110eE>
800013c8:	00002317          	auipc	t1,0x2
800013cc:	2dc30067          	jr	732(t1) # 800036a4 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i32$GT$3fmt17h8b16a5bc167e7410E>

800013d0 <_ZN4core3fmt5Write10write_char17h61427ba210d54f53E.llvm.14510610307159664504>:
800013d0:	ff010113          	addi	sp,sp,-16
800013d4:	08000513          	li	a0,128
800013d8:	00012623          	sw	zero,12(sp)
800013dc:	00a5f863          	bgeu	a1,a0,800013ec <_ZN4core3fmt5Write10write_char17h61427ba210d54f53E.llvm.14510610307159664504+0x1c>
800013e0:	00d10513          	addi	a0,sp,13
800013e4:	00b10623          	sb	a1,12(sp)
800013e8:	0a00006f          	j	80001488 <_ZN4core3fmt5Write10write_char17h61427ba210d54f53E.llvm.14510610307159664504+0xb8>
800013ec:	00b5d513          	srli	a0,a1,0xb
800013f0:	02051263          	bnez	a0,80001414 <_ZN4core3fmt5Write10write_char17h61427ba210d54f53E.llvm.14510610307159664504+0x44>
800013f4:	00e10513          	addi	a0,sp,14
800013f8:	0065d613          	srli	a2,a1,0x6
800013fc:	0c066613          	ori	a2,a2,192
80001400:	00c10623          	sb	a2,12(sp)
80001404:	03f5f593          	andi	a1,a1,63
80001408:	08058593          	addi	a1,a1,128
8000140c:	00b106a3          	sb	a1,13(sp)
80001410:	0780006f          	j	80001488 <_ZN4core3fmt5Write10write_char17h61427ba210d54f53E.llvm.14510610307159664504+0xb8>
80001414:	0105d513          	srli	a0,a1,0x10
80001418:	02051a63          	bnez	a0,8000144c <_ZN4core3fmt5Write10write_char17h61427ba210d54f53E.llvm.14510610307159664504+0x7c>
8000141c:	00f10513          	addi	a0,sp,15
80001420:	00c5d613          	srli	a2,a1,0xc
80001424:	0e066613          	ori	a2,a2,224
80001428:	00c10623          	sb	a2,12(sp)
8000142c:	01459613          	slli	a2,a1,0x14
80001430:	01a65613          	srli	a2,a2,0x1a
80001434:	08060613          	addi	a2,a2,128
80001438:	00c106a3          	sb	a2,13(sp)
8000143c:	03f5f593          	andi	a1,a1,63
80001440:	08058593          	addi	a1,a1,128
80001444:	00b10723          	sb	a1,14(sp)
80001448:	0400006f          	j	80001488 <_ZN4core3fmt5Write10write_char17h61427ba210d54f53E.llvm.14510610307159664504+0xb8>
8000144c:	01010513          	addi	a0,sp,16
80001450:	0125d613          	srli	a2,a1,0x12
80001454:	0f066613          	ori	a2,a2,240
80001458:	00c10623          	sb	a2,12(sp)
8000145c:	00e59613          	slli	a2,a1,0xe
80001460:	01a65613          	srli	a2,a2,0x1a
80001464:	08060613          	addi	a2,a2,128
80001468:	00c106a3          	sb	a2,13(sp)
8000146c:	01459613          	slli	a2,a1,0x14
80001470:	01a65613          	srli	a2,a2,0x1a
80001474:	08060613          	addi	a2,a2,128
80001478:	00c10723          	sb	a2,14(sp)
8000147c:	03f5f593          	andi	a1,a1,63
80001480:	08058593          	addi	a1,a1,128
80001484:	00b107a3          	sb	a1,15(sp)
80001488:	00c10613          	addi	a2,sp,12
8000148c:	100005b7          	lui	a1,0x10000
80001490:	00064683          	lbu	a3,0(a2)
80001494:	00160713          	addi	a4,a2,1
80001498:	00d58023          	sb	a3,0(a1) # 10000000 <.Lline_table_start2+0xfffec61>
8000149c:	00070613          	mv	a2,a4
800014a0:	fea718e3          	bne	a4,a0,80001490 <_ZN4core3fmt5Write10write_char17h61427ba210d54f53E.llvm.14510610307159664504+0xc0>
800014a4:	00000513          	li	a0,0
800014a8:	01010113          	addi	sp,sp,16
800014ac:	00008067          	ret

800014b0 <_ZN4core3fmt5Write9write_fmt17he488f3dd19d82de4E.llvm.14510610307159664504>:
800014b0:	80004637          	lui	a2,0x80004
800014b4:	64460613          	addi	a2,a2,1604 # 80004644 <anon.58335f229ba488831bc287488e11d397.25.llvm.14510610307159664504>
800014b8:	00058693          	mv	a3,a1
800014bc:	00060593          	mv	a1,a2
800014c0:	00068613          	mv	a2,a3
800014c4:	00001317          	auipc	t1,0x1
800014c8:	27830067          	jr	632(t1) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>

800014cc <_ZN53_$LT$core..fmt..Error$u20$as$u20$core..fmt..Debug$GT$3fmt17he79e882de461163bE.llvm.14510610307159664504>:
800014cc:	800046b7          	lui	a3,0x80004
800014d0:	51868693          	addi	a3,a3,1304 # 80004518 <.Lanon.58335f229ba488831bc287488e11d397.4>
800014d4:	00500613          	li	a2,5
800014d8:	00058513          	mv	a0,a1
800014dc:	00068593          	mv	a1,a3
800014e0:	00002317          	auipc	t1,0x2
800014e4:	b5830067          	jr	-1192(t1) # 80003038 <_ZN4core3fmt9Formatter9write_str17hd607abcbb12fb4c8E>

800014e8 <_ZN64_$LT$core..alloc..layout..Layout$u20$as$u20$core..fmt..Debug$GT$3fmt17h3f589c9bfce4375fE>:
800014e8:	fe010113          	addi	sp,sp,-32
800014ec:	00112e23          	sw	ra,28(sp)
800014f0:	00058293          	mv	t0,a1
800014f4:	00450793          	addi	a5,a0,4
800014f8:	00a12c23          	sw	a0,24(sp)
800014fc:	80004537          	lui	a0,0x80004
80001500:	53050513          	addi	a0,a0,1328 # 80004530 <.Lanon.58335f229ba488831bc287488e11d397.6>
80001504:	00a12423          	sw	a0,8(sp)
80001508:	01810513          	addi	a0,sp,24
8000150c:	00a12223          	sw	a0,4(sp)
80001510:	00500513          	li	a0,5
80001514:	800045b7          	lui	a1,0x80004
80001518:	54058593          	addi	a1,a1,1344 # 80004540 <.Lanon.58335f229ba488831bc287488e11d397.7>
8000151c:	800046b7          	lui	a3,0x80004
80001520:	54a68693          	addi	a3,a3,1354 # 8000454a <.Lanon.58335f229ba488831bc287488e11d397.7+0xa>
80001524:	80004837          	lui	a6,0x80004
80001528:	52080813          	addi	a6,a6,1312 # 80004520 <.Lanon.58335f229ba488831bc287488e11d397.5>
8000152c:	800048b7          	lui	a7,0x80004
80001530:	54e88893          	addi	a7,a7,1358 # 8000454e <.Lanon.58335f229ba488831bc287488e11d397.9>
80001534:	00600613          	li	a2,6
80001538:	00400713          	li	a4,4
8000153c:	00a12023          	sw	a0,0(sp)
80001540:	00028513          	mv	a0,t0
80001544:	00002097          	auipc	ra,0x2
80001548:	b20080e7          	jalr	-1248(ra) # 80003064 <_ZN4core3fmt9Formatter26debug_struct_field2_finish17h8b989ef45de6295cE>
8000154c:	01c12083          	lw	ra,28(sp)
80001550:	02010113          	addi	sp,sp,32
80001554:	00008067          	ret

80001558 <_ZN71_$LT$riscv..register..mcause..Exception$u20$as$u20$core..fmt..Debug$GT$3fmt17h43f79eca3d356742E>:
80001558:	00054503          	lbu	a0,0(a0)
8000155c:	00251513          	slli	a0,a0,0x2
80001560:	80004637          	lui	a2,0x80004
80001564:	7f860613          	addi	a2,a2,2040 # 800047f8 <.Lswitch.table._ZN71_$LT$riscv..register..mcause..Exception$u20$as$u20$core..fmt..Debug$GT$3fmt17h43f79eca3d356742E>
80001568:	00a60633          	add	a2,a2,a0
8000156c:	00062603          	lw	a2,0(a2)
80001570:	800056b7          	lui	a3,0x80005
80001574:	83468693          	addi	a3,a3,-1996 # 80004834 <.Lswitch.table._ZN71_$LT$riscv..register..mcause..Exception$u20$as$u20$core..fmt..Debug$GT$3fmt17h43f79eca3d356742E.15>
80001578:	00a68533          	add	a0,a3,a0
8000157c:	00052683          	lw	a3,0(a0)
80001580:	00058513          	mv	a0,a1
80001584:	00068593          	mv	a1,a3
80001588:	00002317          	auipc	t1,0x2
8000158c:	ab030067          	jr	-1360(t1) # 80003038 <_ZN4core3fmt9Formatter9write_str17hd607abcbb12fb4c8E>

80001590 <_ZN61_$LT$SuperOS..printer..Writer$u20$as$u20$core..fmt..Write$GT$9write_str17h8b24a3efbe14ea2aE.llvm.14510610307159664504>:
80001590:	00060e63          	beqz	a2,800015ac <_ZN61_$LT$SuperOS..printer..Writer$u20$as$u20$core..fmt..Write$GT$9write_str17h8b24a3efbe14ea2aE.llvm.14510610307159664504+0x1c>
80001594:	10000537          	lui	a0,0x10000
80001598:	0005c683          	lbu	a3,0(a1)
8000159c:	00158593          	addi	a1,a1,1
800015a0:	fff60613          	addi	a2,a2,-1
800015a4:	00d50023          	sb	a3,0(a0) # 10000000 <.Lline_table_start2+0xfffec61>
800015a8:	fe0618e3          	bnez	a2,80001598 <_ZN61_$LT$SuperOS..printer..Writer$u20$as$u20$core..fmt..Write$GT$9write_str17h8b24a3efbe14ea2aE.llvm.14510610307159664504+0x8>
800015ac:	00000513          	li	a0,0
800015b0:	00008067          	ret

800015b4 <rust_begin_unwind>:
800015b4:	fd010113          	addi	sp,sp,-48
800015b8:	02112623          	sw	ra,44(sp)
800015bc:	00a12223          	sw	a0,4(sp)
800015c0:	00410513          	addi	a0,sp,4
800015c4:	02a12023          	sw	a0,32(sp)
800015c8:	80001537          	lui	a0,0x80001
800015cc:	81850513          	addi	a0,a0,-2024 # 80000818 <_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h02ba342068fcc2deE>
800015d0:	02a12223          	sw	a0,36(sp)
800015d4:	80004537          	lui	a0,0x80004
800015d8:	75050513          	addi	a0,a0,1872 # 80004750 <.Lanon.58335f229ba488831bc287488e11d397.40>
800015dc:	00a12423          	sw	a0,8(sp)
800015e0:	00200513          	li	a0,2
800015e4:	00a12623          	sw	a0,12(sp)
800015e8:	00012c23          	sw	zero,24(sp)
800015ec:	02010513          	addi	a0,sp,32
800015f0:	00a12823          	sw	a0,16(sp)
800015f4:	00100513          	li	a0,1
800015f8:	00a12a23          	sw	a0,20(sp)
800015fc:	800045b7          	lui	a1,0x80004
80001600:	64458593          	addi	a1,a1,1604 # 80004644 <anon.58335f229ba488831bc287488e11d397.25.llvm.14510610307159664504>
80001604:	02b10513          	addi	a0,sp,43
80001608:	00810613          	addi	a2,sp,8
8000160c:	00001097          	auipc	ra,0x1
80001610:	130080e7          	jalr	304(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
80001614:	00051463          	bnez	a0,8000161c <rust_begin_unwind+0x68>
80001618:	0000006f          	j	80001618 <rust_begin_unwind+0x64>
8000161c:	80004537          	lui	a0,0x80004
80001620:	66c50513          	addi	a0,a0,1644 # 8000466c <anon.58335f229ba488831bc287488e11d397.27.llvm.14510610307159664504>
80001624:	800046b7          	lui	a3,0x80004
80001628:	65c68693          	addi	a3,a3,1628 # 8000465c <anon.58335f229ba488831bc287488e11d397.26.llvm.14510610307159664504>
8000162c:	80004737          	lui	a4,0x80004
80001630:	6a870713          	addi	a4,a4,1704 # 800046a8 <anon.58335f229ba488831bc287488e11d397.29.llvm.14510610307159664504>
80001634:	02b00593          	li	a1,43
80001638:	02b10613          	addi	a2,sp,43
8000163c:	00001097          	auipc	ra,0x1
80001640:	ac0080e7          	jalr	-1344(ra) # 800020fc <_ZN4core6result13unwrap_failed17h987d8f67a7161eb1E>

80001644 <kernel_main>:
80001644:	f1010113          	addi	sp,sp,-240
80001648:	0e112623          	sw	ra,236(sp)
8000164c:	0e812423          	sw	s0,232(sp)
80001650:	0e912223          	sw	s1,228(sp)
80001654:	0f212023          	sw	s2,224(sp)
80001658:	0d312e23          	sw	s3,220(sp)
8000165c:	0d412c23          	sw	s4,216(sp)
80001660:	0d512a23          	sw	s5,212(sp)
80001664:	0d612823          	sw	s6,208(sp)
80001668:	0d712623          	sw	s7,204(sp)
8000166c:	0d812423          	sw	s8,200(sp)
80001670:	0d912223          	sw	s9,196(sp)
80001674:	0da12023          	sw	s10,192(sp)
80001678:	0bb12e23          	sw	s11,188(sp)
8000167c:	10000537          	lui	a0,0x10000
80001680:	07300593          	li	a1,115
80001684:	00b50023          	sb	a1,0(a0) # 10000000 <.Lline_table_start2+0xfffec61>
80001688:	07400613          	li	a2,116
8000168c:	00c50023          	sb	a2,0(a0)
80001690:	06100693          	li	a3,97
80001694:	00d50023          	sb	a3,0(a0)
80001698:	07200693          	li	a3,114
8000169c:	00d50023          	sb	a3,0(a0)
800016a0:	00c50023          	sb	a2,0(a0)
800016a4:	06500613          	li	a2,101
800016a8:	00c50023          	sb	a2,0(a0)
800016ac:	06400613          	li	a2,100
800016b0:	00c50023          	sb	a2,0(a0)
800016b4:	02100613          	li	a2,33
800016b8:	00c50023          	sb	a2,0(a0)
800016bc:	00a00613          	li	a2,10
800016c0:	00c50023          	sb	a2,0(a0)
800016c4:	04400693          	li	a3,68
800016c8:	00d50023          	sb	a3,0(a0)
800016cc:	04f00693          	li	a3,79
800016d0:	00d50023          	sb	a3,0(a0)
800016d4:	06f00713          	li	a4,111
800016d8:	00e50023          	sb	a4,0(a0)
800016dc:	00d50023          	sb	a3,0(a0)
800016e0:	04d00693          	li	a3,77
800016e4:	00d50023          	sb	a3,0(a0)
800016e8:	02000693          	li	a3,32
800016ec:	00d50023          	sb	a3,0(a0)
800016f0:	00e50023          	sb	a4,0(a0)
800016f4:	00b50023          	sb	a1,0(a0)
800016f8:	00c50023          	sb	a2,0(a0)
800016fc:	00000097          	auipc	ra,0x0
80001700:	8f4080e7          	jalr	-1804(ra) # 80000ff0 <_ZN7SuperOS6palloc4init17hb4eb3dcde82ecf87E>
80001704:	80006437          	lui	s0,0x80006
80001708:	00040413          	mv	s0,s0
8000170c:	0a812223          	sw	s0,164(sp)
80001710:	0a410513          	addi	a0,sp,164
80001714:	08a12623          	sw	a0,140(sp)
80001718:	80001537          	lui	a0,0x80001
8000171c:	82450513          	addi	a0,a0,-2012 # 80000824 <_ZN54_$LT$$BP$const$u20$T$u20$as$u20$core..fmt..Pointer$GT$3fmt17hef7da24cb2e3fea3E>
80001720:	08a12823          	sw	a0,144(sp)
80001724:	80004537          	lui	a0,0x80004
80001728:	6fc50513          	addi	a0,a0,1788 # 800046fc <.Lanon.58335f229ba488831bc287488e11d397.35>
8000172c:	00a12223          	sw	a0,4(sp)
80001730:	00200513          	li	a0,2
80001734:	00a12423          	sw	a0,8(sp)
80001738:	00012a23          	sw	zero,20(sp)
8000173c:	08c10513          	addi	a0,sp,140
80001740:	00a12623          	sw	a0,12(sp)
80001744:	00100513          	li	a0,1
80001748:	00a12823          	sw	a0,16(sp)
8000174c:	800045b7          	lui	a1,0x80004
80001750:	64458593          	addi	a1,a1,1604 # 80004644 <anon.58335f229ba488831bc287488e11d397.25.llvm.14510610307159664504>
80001754:	0b810513          	addi	a0,sp,184
80001758:	00410613          	addi	a2,sp,4
8000175c:	00001097          	auipc	ra,0x1
80001760:	fe0080e7          	jalr	-32(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
80001764:	18051063          	bnez	a0,800018e4 <kernel_main+0x2a0>
80001768:	80006537          	lui	a0,0x80006
8000176c:	f2052823          	sw	zero,-208(a0) # 80005f30 <_ZN7SuperOS6kalloc16KERNEL_ALLOCATOR17h0e08997bf1bee0d5E.llvm.14510610307159664504>
80001770:	f3050513          	addi	a0,a0,-208
80001774:	00340593          	addi	a1,s0,3 # 80006003 <KALLOC_BUFFER+0x3>
80001778:	ffc5f593          	andi	a1,a1,-4
8000177c:	01e00637          	lui	a2,0x1e00
80001780:	00c40633          	add	a2,s0,a2
80001784:	40b60633          	sub	a2,a2,a1
80001788:	020006b7          	lui	a3,0x2000
8000178c:	ffc68693          	addi	a3,a3,-4 # 1fffffc <.Lline_table_start2+0x1ffec5d>
80001790:	00d676b3          	and	a3,a2,a3
80001794:	00d5a023          	sw	a3,0(a1)
80001798:	0005a223          	sw	zero,4(a1)
8000179c:	00d586b3          	add	a3,a1,a3
800017a0:	00367613          	andi	a2,a2,3
800017a4:	00052223          	sw	zero,4(a0)
800017a8:	00b52423          	sw	a1,8(a0)
800017ac:	00b52623          	sw	a1,12(a0)
800017b0:	00d52823          	sw	a3,16(a0)
800017b4:	00c50a23          	sb	a2,20(a0)
800017b8:	80001537          	lui	a0,0x80001
800017bc:	2d050513          	addi	a0,a0,720 # 800012d0 <user_trap>
800017c0:	30551073          	.insn	4, 0x30551073
800017c4:	30002573          	.insn	4, 0x30002573
800017c8:	00300593          	li	a1,3
800017cc:	00b59593          	slli	a1,a1,0xb
800017d0:	00b56533          	or	a0,a0,a1
800017d4:	30051073          	.insn	4, 0x30051073
800017d8:	08000513          	li	a0,128
800017dc:	30052073          	.insn	4, 0x30052073
800017e0:	80005537          	lui	a0,0x80005
800017e4:	f2c54583          	lbu	a1,-212(a0) # 80004f2c <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.3.llvm.10321614113892589689>
800017e8:	00058463          	beqz	a1,800017f0 <kernel_main+0x1ac>
800017ec:	0000006f          	j	800017ec <kernel_main+0x1a8>
800017f0:	800055b7          	lui	a1,0x80005
800017f4:	f285a603          	lw	a2,-216(a1) # 80004f28 <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.2.llvm.10321614113892589689>
800017f8:	800056b7          	lui	a3,0x80005
800017fc:	f206a683          	lw	a3,-224(a3) # 80004f20 <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.0.llvm.10321614113892589689>
80001800:	00100713          	li	a4,1
80001804:	f2e50623          	sb	a4,-212(a0)
80001808:	00160613          	addi	a2,a2,1 # 1e00001 <.Lline_table_start2+0x1dfec62>
8000180c:	f2c5a423          	sw	a2,-216(a1)
80001810:	02068463          	beqz	a3,80001838 <kernel_main+0x1f4>
80001814:	80005537          	lui	a0,0x80005
80001818:	f2452403          	lw	s0,-220(a0) # 80004f24 <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.1.llvm.10321614113892589689>
8000181c:	00c41413          	slli	s0,s0,0xc
80001820:	28040a63          	beqz	s0,80001ab4 <kernel_main+0x470>
80001824:	00042503          	lw	a0,0(s0)
80001828:	02050263          	beqz	a0,8000184c <kernel_main+0x208>
8000182c:	00442503          	lw	a0,4(s0)
80001830:	00100593          	li	a1,1
80001834:	01c0006f          	j	80001850 <kernel_main+0x20c>
80001838:	f2050623          	sb	zero,-212(a0)
8000183c:	80004537          	lui	a0,0x80004
80001840:	76c50513          	addi	a0,a0,1900 # 8000476c <.Lanon.58335f229ba488831bc287488e11d397.44>
80001844:	00000097          	auipc	ra,0x0
80001848:	6d0080e7          	jalr	1744(ra) # 80001f14 <_ZN4core6option13unwrap_failed17ha917ca27cfe8d772E>
8000184c:	00000593          	li	a1,0
80001850:	80005637          	lui	a2,0x80005
80001854:	f2b62023          	sw	a1,-224(a2) # 80004f20 <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.0.llvm.10321614113892589689>
80001858:	800055b7          	lui	a1,0x80005
8000185c:	f2a5a223          	sw	a0,-220(a1) # 80004f24 <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.1.llvm.10321614113892589689>
80001860:	80005537          	lui	a0,0x80005
80001864:	f2050623          	sb	zero,-212(a0) # 80004f2c <_ZN7SuperOS6palloc14PAGE_ALLOCATOR17hc506b740a132e98fE.3.llvm.10321614113892589689>
80001868:	08010493          	addi	s1,sp,128
8000186c:	00410513          	addi	a0,sp,4
80001870:	07c00613          	li	a2,124
80001874:	00000593          	li	a1,0
80001878:	00002097          	auipc	ra,0x2
8000187c:	268080e7          	jalr	616(ra) # 80003ae0 <memset>
80001880:	80002537          	lui	a0,0x80002
80001884:	ac450513          	addi	a0,a0,-1340 # 80001ac4 <_ZN7SuperOS9user_main17h72676f5f18d00749E>
80001888:	08a12023          	sw	a0,128(sp)
8000188c:	08012223          	sw	zero,132(sp)
80001890:	0a912223          	sw	s1,164(sp)
80001894:	80003537          	lui	a0,0x80003
80001898:	62050513          	addi	a0,a0,1568 # 80003620 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hcc6fe785c9e9110eE>
8000189c:	0aa12423          	sw	a0,168(sp)
800018a0:	80004537          	lui	a0,0x80004
800018a4:	78850513          	addi	a0,a0,1928 # 80004788 <.Lanon.58335f229ba488831bc287488e11d397.46>
800018a8:	08a12623          	sw	a0,140(sp)
800018ac:	00200513          	li	a0,2
800018b0:	08a12823          	sw	a0,144(sp)
800018b4:	08012e23          	sw	zero,156(sp)
800018b8:	0a410513          	addi	a0,sp,164
800018bc:	08a12a23          	sw	a0,148(sp)
800018c0:	00100513          	li	a0,1
800018c4:	08a12c23          	sw	a0,152(sp)
800018c8:	800045b7          	lui	a1,0x80004
800018cc:	64458593          	addi	a1,a1,1604 # 80004644 <anon.58335f229ba488831bc287488e11d397.25.llvm.14510610307159664504>
800018d0:	0b810513          	addi	a0,sp,184
800018d4:	08c10613          	addi	a2,sp,140
800018d8:	00001097          	auipc	ra,0x1
800018dc:	e64080e7          	jalr	-412(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
800018e0:	02050663          	beqz	a0,8000190c <kernel_main+0x2c8>
800018e4:	80004537          	lui	a0,0x80004
800018e8:	66c50513          	addi	a0,a0,1644 # 8000466c <anon.58335f229ba488831bc287488e11d397.27.llvm.14510610307159664504>
800018ec:	800046b7          	lui	a3,0x80004
800018f0:	65c68693          	addi	a3,a3,1628 # 8000465c <anon.58335f229ba488831bc287488e11d397.26.llvm.14510610307159664504>
800018f4:	80004737          	lui	a4,0x80004
800018f8:	6a870713          	addi	a4,a4,1704 # 800046a8 <anon.58335f229ba488831bc287488e11d397.29.llvm.14510610307159664504>
800018fc:	02b00593          	li	a1,43
80001900:	0b810613          	addi	a2,sp,184
80001904:	00000097          	auipc	ra,0x0
80001908:	7f8080e7          	jalr	2040(ra) # 800020fc <_ZN4core6result13unwrap_failed17h987d8f67a7161eb1E>
8000190c:	00001537          	lui	a0,0x1
80001910:	ff850513          	addi	a0,a0,-8 # ff8 <.Lline_table_start1+0x30>
80001914:	00a46533          	or	a0,s0,a0
80001918:	00a12423          	sw	a0,8(sp)
8000191c:	10000a37          	lui	s4,0x10000
80001920:	04800513          	li	a0,72
80001924:	00aa0023          	sb	a0,0(s4) # 10000000 <.Lline_table_start2+0xfffec61>
80001928:	06500a93          	li	s5,101
8000192c:	015a0023          	sb	s5,0(s4)
80001930:	06c00513          	li	a0,108
80001934:	00aa0023          	sb	a0,0(s4)
80001938:	00aa0023          	sb	a0,0(s4)
8000193c:	06f00593          	li	a1,111
80001940:	00ba0023          	sb	a1,0(s4)
80001944:	02000613          	li	a2,32
80001948:	00ca0023          	sb	a2,0(s4)
8000194c:	07700613          	li	a2,119
80001950:	00ca0023          	sb	a2,0(s4)
80001954:	00ba0023          	sb	a1,0(s4)
80001958:	07200b93          	li	s7,114
8000195c:	017a0023          	sb	s7,0(s4)
80001960:	00aa0023          	sb	a0,0(s4)
80001964:	06400513          	li	a0,100
80001968:	00aa0023          	sb	a0,0(s4)
8000196c:	02100513          	li	a0,33
80001970:	00aa0023          	sb	a0,0(s4)
80001974:	00a00513          	li	a0,10
80001978:	00aa0023          	sb	a0,0(s4)
8000197c:	00f00c93          	li	s9,15
80001980:	0b410d93          	addi	s11,sp,180
80001984:	08b10d13          	addi	s10,sp,139
80001988:	80001b37          	lui	s6,0x80001
8000198c:	558b0b13          	addi	s6,s6,1368 # 80001558 <_ZN71_$LT$riscv..register..mcause..Exception$u20$as$u20$core..fmt..Debug$GT$3fmt17h43f79eca3d356742E>
80001990:	80004c37          	lui	s8,0x80004
80001994:	6d0c0c13          	addi	s8,s8,1744 # 800046d0 <.Lanon.58335f229ba488831bc287488e11d397.33>
80001998:	00300493          	li	s1,3
8000199c:	80004437          	lui	s0,0x80004
800019a0:	64440413          	addi	s0,s0,1604 # 80004644 <anon.58335f229ba488831bc287488e11d397.25.llvm.14510610307159664504>
800019a4:	07500913          	li	s2,117
800019a8:	07400993          	li	s3,116
800019ac:	0740006f          	j	80001a20 <kernel_main+0x3dc>
800019b0:	06d00513          	li	a0,109
800019b4:	00aa0023          	sb	a0,0(s4)
800019b8:	06300513          	li	a0,99
800019bc:	00aa0023          	sb	a0,0(s4)
800019c0:	06100513          	li	a0,97
800019c4:	00aa0023          	sb	a0,0(s4)
800019c8:	012a0023          	sb	s2,0(s4)
800019cc:	07300513          	li	a0,115
800019d0:	00aa0023          	sb	a0,0(s4)
800019d4:	015a0023          	sb	s5,0(s4)
800019d8:	03a00513          	li	a0,58
800019dc:	00aa0023          	sb	a0,0(s4)
800019e0:	02000513          	li	a0,32
800019e4:	00aa0023          	sb	a0,0(s4)
800019e8:	06900513          	li	a0,105
800019ec:	00aa0023          	sb	a0,0(s4)
800019f0:	06e00513          	li	a0,110
800019f4:	00aa0023          	sb	a0,0(s4)
800019f8:	013a0023          	sb	s3,0(s4)
800019fc:	015a0023          	sb	s5,0(s4)
80001a00:	017a0023          	sb	s7,0(s4)
80001a04:	017a0023          	sb	s7,0(s4)
80001a08:	012a0023          	sb	s2,0(s4)
80001a0c:	07000513          	li	a0,112
80001a10:	00aa0023          	sb	a0,0(s4)
80001a14:	013a0023          	sb	s3,0(s4)
80001a18:	00a00513          	li	a0,10
80001a1c:	00aa0023          	sb	a0,0(s4)
80001a20:	00410513          	addi	a0,sp,4
80001a24:	fffff097          	auipc	ra,0xfffff
80001a28:	7dc080e7          	jalr	2012(ra) # 80001200 <run_user>
80001a2c:	34202573          	.insn	4, 0x34202573
80001a30:	f80540e3          	bltz	a0,800019b0 <kernel_main+0x36c>
80001a34:	00e00593          	li	a1,14
80001a38:	00acea63          	bltu	s9,a0,80001a4c <kernel_main+0x408>
80001a3c:	800045b7          	lui	a1,0x80004
80001a40:	58858593          	addi	a1,a1,1416 # 80004588 <.Lanon.58335f229ba488831bc287488e11d397.10+0x35>
80001a44:	00a58533          	add	a0,a1,a0
80001a48:	00054583          	lbu	a1,0(a0)
80001a4c:	08b105a3          	sb	a1,139(sp)
80001a50:	34102573          	.insn	4, 0x34102573
80001a54:	0aa12a23          	sw	a0,180(sp)
80001a58:	0bb12223          	sw	s11,164(sp)
80001a5c:	80003537          	lui	a0,0x80003
80001a60:	62050513          	addi	a0,a0,1568 # 80003620 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hcc6fe785c9e9110eE>
80001a64:	0aa12423          	sw	a0,168(sp)
80001a68:	0ba12623          	sw	s10,172(sp)
80001a6c:	0b612823          	sw	s6,176(sp)
80001a70:	09812623          	sw	s8,140(sp)
80001a74:	08912823          	sw	s1,144(sp)
80001a78:	08012e23          	sw	zero,156(sp)
80001a7c:	0a410513          	addi	a0,sp,164
80001a80:	08a12a23          	sw	a0,148(sp)
80001a84:	00200513          	li	a0,2
80001a88:	08a12c23          	sw	a0,152(sp)
80001a8c:	0b810513          	addi	a0,sp,184
80001a90:	08c10613          	addi	a2,sp,140
80001a94:	00040593          	mv	a1,s0
80001a98:	00001097          	auipc	ra,0x1
80001a9c:	ca4080e7          	jalr	-860(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
80001aa0:	e40512e3          	bnez	a0,800018e4 <kernel_main+0x2a0>
80001aa4:	08012503          	lw	a0,128(sp)
80001aa8:	00450513          	addi	a0,a0,4
80001aac:	08a12023          	sw	a0,128(sp)
80001ab0:	f71ff06f          	j	80001a20 <kernel_main+0x3dc>
80001ab4:	80004537          	lui	a0,0x80004
80001ab8:	4e850513          	addi	a0,a0,1256 # 800044e8 <anon.272ad12a150edddff8aff02f5f98f349.30.llvm.10321614113892589689>
80001abc:	00000097          	auipc	ra,0x0
80001ac0:	458080e7          	jalr	1112(ra) # 80001f14 <_ZN4core6option13unwrap_failed17ha917ca27cfe8d772E>

80001ac4 <_ZN7SuperOS9user_main17h72676f5f18d00749E>:
80001ac4:	f8010113          	addi	sp,sp,-128
80001ac8:	06112e23          	sw	ra,124(sp)
80001acc:	06812c23          	sw	s0,120(sp)
80001ad0:	06912a23          	sw	s1,116(sp)
80001ad4:	07212823          	sw	s2,112(sp)
80001ad8:	07312623          	sw	s3,108(sp)
80001adc:	07412423          	sw	s4,104(sp)
80001ae0:	07512223          	sw	s5,100(sp)
80001ae4:	07612023          	sw	s6,96(sp)
80001ae8:	05712e23          	sw	s7,92(sp)
80001aec:	05812c23          	sw	s8,88(sp)
80001af0:	05912a23          	sw	s9,84(sp)
80001af4:	05a12823          	sw	s10,80(sp)
80001af8:	05b12623          	sw	s11,76(sp)
80001afc:	100004b7          	lui	s1,0x10000
80001b00:	3e800a93          	li	s5,1000
80001b04:	c28f6537          	lui	a0,0xc28f6
80001b08:	c2950b13          	addi	s6,a0,-983 # c28f5c29 <KALLOC_BUFFER+0x428efc29>
80001b0c:	028f6537          	lui	a0,0x28f6
80001b10:	c2850b93          	addi	s7,a0,-984 # 28f5c28 <.Lline_table_start2+0x28f4889>
80001b14:	02410c93          	addi	s9,sp,36
80001b18:	80004db7          	lui	s11,0x80004
80001b1c:	820d8d93          	addi	s11,s11,-2016 # 80003820 <_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17he9e9e363faaccf29E>
80001b20:	80004937          	lui	s2,0x80004
80001b24:	72890913          	addi	s2,s2,1832 # 80004728 <.Lanon.58335f229ba488831bc287488e11d397.38>
80001b28:	00200993          	li	s3,2
80001b2c:	04010a13          	addi	s4,sp,64
80001b30:	00100c13          	li	s8,1
80001b34:	80004437          	lui	s0,0x80004
80001b38:	64440413          	addi	s0,s0,1604 # 80004644 <anon.58335f229ba488831bc287488e11d397.25.llvm.14510610307159664504>
80001b3c:	04800513          	li	a0,72
80001b40:	00a48023          	sb	a0,0(s1) # 10000000 <.Lline_table_start2+0xfffec61>
80001b44:	06500513          	li	a0,101
80001b48:	00a48023          	sb	a0,0(s1)
80001b4c:	06c00593          	li	a1,108
80001b50:	00b48023          	sb	a1,0(s1)
80001b54:	00b48023          	sb	a1,0(s1)
80001b58:	06f00593          	li	a1,111
80001b5c:	00b48023          	sb	a1,0(s1)
80001b60:	02000593          	li	a1,32
80001b64:	00b48023          	sb	a1,0(s1)
80001b68:	07500593          	li	a1,117
80001b6c:	00b48023          	sb	a1,0(s1)
80001b70:	07300593          	li	a1,115
80001b74:	00b48023          	sb	a1,0(s1)
80001b78:	00a48023          	sb	a0,0(s1)
80001b7c:	07200513          	li	a0,114
80001b80:	00a48023          	sb	a0,0(s1)
80001b84:	00b48023          	sb	a1,0(s1)
80001b88:	02100513          	li	a0,33
80001b8c:	00a48023          	sb	a0,0(s1)
80001b90:	00a48023          	sb	a0,0(s1)
80001b94:	00a48023          	sb	a0,0(s1)
80001b98:	00a00513          	li	a0,10
80001b9c:	00a48023          	sb	a0,0(s1)
80001ba0:	b0002573          	.insn	4, 0xb0002573
80001ba4:	40a00533          	neg	a0,a0
80001ba8:	00a12623          	sw	a0,12(sp)
80001bac:	b0202573          	.insn	4, 0xb0202573
80001bb0:	00000593          	li	a1,0
80001bb4:	40a00533          	neg	a0,a0
80001bb8:	00a12823          	sw	a0,16(sp)
80001bbc:	00012a23          	sw	zero,20(sp)
80001bc0:	00012e23          	sw	zero,28(sp)
80001bc4:	00158d13          	addi	s10,a1,1
80001bc8:	01410513          	addi	a0,sp,20
80001bcc:	fffff097          	auipc	ra,0xfffff
80001bd0:	c64080e7          	jalr	-924(ra) # 80000830 <_ZN5alloc11collections5btree3map25BTreeMap$LT$K$C$V$C$A$GT$6insert17h27e65db92cff35efE>
80001bd4:	000d0593          	mv	a1,s10
80001bd8:	ff5d16e3          	bne	s10,s5,80001bc4 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x100>
80001bdc:	00000d13          	li	s10,0
80001be0:	02012223          	sw	zero,36(sp)
80001be4:	0180006f          	j	80001bfc <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x138>
80001be8:	02412503          	lw	a0,36(sp)
80001bec:	00150513          	addi	a0,a0,1
80001bf0:	02a12223          	sw	a0,36(sp)
80001bf4:	001d0d13          	addi	s10,s10,1
80001bf8:	0d5d0263          	beq	s10,s5,80001cbc <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x1f8>
80001bfc:	02412503          	lw	a0,36(sp)
80001c00:	03650533          	mul	a0,a0,s6
80001c04:	01e51593          	slli	a1,a0,0x1e
80001c08:	00255513          	srli	a0,a0,0x2
80001c0c:	00b56533          	or	a0,a0,a1
80001c10:	02abec63          	bltu	s7,a0,80001c48 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x184>
80001c14:	05912023          	sw	s9,64(sp)
80001c18:	05b12223          	sw	s11,68(sp)
80001c1c:	03212423          	sw	s2,40(sp)
80001c20:	03312623          	sw	s3,44(sp)
80001c24:	02012c23          	sw	zero,56(sp)
80001c28:	03412823          	sw	s4,48(sp)
80001c2c:	03812a23          	sw	s8,52(sp)
80001c30:	04b10513          	addi	a0,sp,75
80001c34:	02810613          	addi	a2,sp,40
80001c38:	00040593          	mv	a1,s0
80001c3c:	00001097          	auipc	ra,0x1
80001c40:	b00080e7          	jalr	-1280(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
80001c44:	12051c63          	bnez	a0,80001d7c <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x2b8>
80001c48:	01412503          	lw	a0,20(sp)
80001c4c:	fa0504e3          	beqz	a0,80001bf4 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x130>
80001c50:	01812583          	lw	a1,24(sp)
80001c54:	03255683          	lhu	a3,50(a0)
80001c58:	00450793          	addi	a5,a0,4
80001c5c:	00269713          	slli	a4,a3,0x2
80001c60:	fff00613          	li	a2,-1
80001c64:	02070e63          	beqz	a4,80001ca0 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x1dc>
80001c68:	0007a803          	lw	a6,0(a5) # 1000 <.Lline_table_start1+0x38>
80001c6c:	00478793          	addi	a5,a5,4
80001c70:	010d38b3          	sltu	a7,s10,a6
80001c74:	010d4833          	xor	a6,s10,a6
80001c78:	01003833          	snez	a6,a6
80001c7c:	411008b3          	neg	a7,a7
80001c80:	0108e833          	or	a6,a7,a6
80001c84:	ffc70713          	addi	a4,a4,-4
80001c88:	00160613          	addi	a2,a2,1
80001c8c:	fd880ce3          	beq	a6,s8,80001c64 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x1a0>
80001c90:	0ff87693          	zext.b	a3,a6
80001c94:	f4068ae3          	beqz	a3,80001be8 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x124>
80001c98:	00059863          	bnez	a1,80001ca8 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x1e4>
80001c9c:	f59ff06f          	j	80001bf4 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x130>
80001ca0:	00068613          	mv	a2,a3
80001ca4:	f40588e3          	beqz	a1,80001bf4 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x130>
80001ca8:	00261613          	slli	a2,a2,0x2
80001cac:	00c50533          	add	a0,a0,a2
80001cb0:	03452503          	lw	a0,52(a0)
80001cb4:	fff58593          	addi	a1,a1,-1
80001cb8:	f9dff06f          	j	80001c54 <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x190>
80001cbc:	05912023          	sw	s9,64(sp)
80001cc0:	05b12223          	sw	s11,68(sp)
80001cc4:	03212423          	sw	s2,40(sp)
80001cc8:	03312623          	sw	s3,44(sp)
80001ccc:	02012c23          	sw	zero,56(sp)
80001cd0:	03412823          	sw	s4,48(sp)
80001cd4:	03812a23          	sw	s8,52(sp)
80001cd8:	04b10513          	addi	a0,sp,75
80001cdc:	02810613          	addi	a2,sp,40
80001ce0:	00040593          	mv	a1,s0
80001ce4:	00001097          	auipc	ra,0x1
80001ce8:	a58080e7          	jalr	-1448(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
80001cec:	08051863          	bnez	a0,80001d7c <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x2b8>
80001cf0:	01410513          	addi	a0,sp,20
80001cf4:	fffff097          	auipc	ra,0xfffff
80001cf8:	ca0080e7          	jalr	-864(ra) # 80000994 <_ZN99_$LT$alloc..collections..btree..map..BTreeMap$LT$K$C$V$C$A$GT$$u20$as$u20$core..ops..drop..Drop$GT$4drop17heea398eafd479a40E>
80001cfc:	b0002573          	.insn	4, 0xb0002573
80001d00:	00c12583          	lw	a1,12(sp)
80001d04:	00a58533          	add	a0,a1,a0
80001d08:	00a12623          	sw	a0,12(sp)
80001d0c:	b0202573          	.insn	4, 0xb0202573
80001d10:	01012583          	lw	a1,16(sp)
80001d14:	00a58533          	add	a0,a1,a0
80001d18:	00a12823          	sw	a0,16(sp)
80001d1c:	00c10513          	addi	a0,sp,12
80001d20:	00a12a23          	sw	a0,20(sp)
80001d24:	01b12c23          	sw	s11,24(sp)
80001d28:	01010513          	addi	a0,sp,16
80001d2c:	00a12e23          	sw	a0,28(sp)
80001d30:	03b12023          	sw	s11,32(sp)
80001d34:	80004537          	lui	a0,0x80004
80001d38:	7a850513          	addi	a0,a0,1960 # 800047a8 <.Lanon.58335f229ba488831bc287488e11d397.49>
80001d3c:	02a12423          	sw	a0,40(sp)
80001d40:	00300513          	li	a0,3
80001d44:	02a12623          	sw	a0,44(sp)
80001d48:	02012c23          	sw	zero,56(sp)
80001d4c:	01410513          	addi	a0,sp,20
80001d50:	02a12823          	sw	a0,48(sp)
80001d54:	03312a23          	sw	s3,52(sp)
80001d58:	04b10513          	addi	a0,sp,75
80001d5c:	02810613          	addi	a2,sp,40
80001d60:	00040593          	mv	a1,s0
80001d64:	00001097          	auipc	ra,0x1
80001d68:	9d8080e7          	jalr	-1576(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
80001d6c:	00051863          	bnez	a0,80001d7c <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x2b8>
80001d70:	00000073          	ecall
80001d74:	0015200f          	cbo.clean	(a0)
80001d78:	dc5ff06f          	j	80001b3c <_ZN7SuperOS9user_main17h72676f5f18d00749E+0x78>
80001d7c:	80004537          	lui	a0,0x80004
80001d80:	66c50513          	addi	a0,a0,1644 # 8000466c <anon.58335f229ba488831bc287488e11d397.27.llvm.14510610307159664504>
80001d84:	800046b7          	lui	a3,0x80004
80001d88:	65c68693          	addi	a3,a3,1628 # 8000465c <anon.58335f229ba488831bc287488e11d397.26.llvm.14510610307159664504>
80001d8c:	80004737          	lui	a4,0x80004
80001d90:	6a870713          	addi	a4,a4,1704 # 800046a8 <anon.58335f229ba488831bc287488e11d397.29.llvm.14510610307159664504>
80001d94:	02b00593          	li	a1,43
80001d98:	04b10613          	addi	a2,sp,75
80001d9c:	00000097          	auipc	ra,0x0
80001da0:	360080e7          	jalr	864(ra) # 800020fc <_ZN4core6result13unwrap_failed17h987d8f67a7161eb1E>

80001da4 <__rust_dealloc>:
80001da4:	800066b7          	lui	a3,0x80006
80001da8:	f3068693          	addi	a3,a3,-208 # 80005f30 <_ZN7SuperOS6kalloc16KERNEL_ALLOCATOR17h0e08997bf1bee0d5E.llvm.14510610307159664504>
80001dac:	00058713          	mv	a4,a1
80001db0:	00050593          	mv	a1,a0
80001db4:	00068513          	mv	a0,a3
80001db8:	00070693          	mv	a3,a4
80001dbc:	fffff317          	auipc	t1,0xfffff
80001dc0:	f6030067          	jr	-160(t1) # 80000d1c <_ZN95_$LT$SuperOS..linked_list_allocator..LockedHeap$u20$as$u20$core..alloc..global..GlobalAlloc$GT$7dealloc17h5936be95759e6178E>

80001dc4 <_ZN7SuperOS6kalloc18handle_alloc_error17hc1197898e00c3e6fE>:
80001dc4:	fd010113          	addi	sp,sp,-48
80001dc8:	00a12423          	sw	a0,8(sp)
80001dcc:	00b12623          	sw	a1,12(sp)
80001dd0:	00810513          	addi	a0,sp,8
80001dd4:	02a12423          	sw	a0,40(sp)
80001dd8:	80001537          	lui	a0,0x80001
80001ddc:	4e850513          	addi	a0,a0,1256 # 800014e8 <_ZN64_$LT$core..alloc..layout..Layout$u20$as$u20$core..fmt..Debug$GT$3fmt17h3f589c9bfce4375fE>
80001de0:	02a12623          	sw	a0,44(sp)
80001de4:	80004537          	lui	a0,0x80004
80001de8:	7d050513          	addi	a0,a0,2000 # 800047d0 <.Lanon.58335f229ba488831bc287488e11d397.51>
80001dec:	00a12823          	sw	a0,16(sp)
80001df0:	00100513          	li	a0,1
80001df4:	00a12a23          	sw	a0,20(sp)
80001df8:	02012023          	sw	zero,32(sp)
80001dfc:	02810593          	addi	a1,sp,40
80001e00:	00b12c23          	sw	a1,24(sp)
80001e04:	00a12e23          	sw	a0,28(sp)
80001e08:	800045b7          	lui	a1,0x80004
80001e0c:	7e858593          	addi	a1,a1,2024 # 800047e8 <.Lanon.58335f229ba488831bc287488e11d397.53>
80001e10:	01010513          	addi	a0,sp,16
80001e14:	00000097          	auipc	ra,0x0
80001e18:	274080e7          	jalr	628(ra) # 80002088 <_ZN4core9panicking9panic_fmt17hd44f1c16c40b716eE>

80001e1c <__rg_oom>:
80001e1c:	00050613          	mv	a2,a0
80001e20:	00058513          	mv	a0,a1
80001e24:	00060593          	mv	a1,a2
80001e28:	00000097          	auipc	ra,0x0
80001e2c:	f9c080e7          	jalr	-100(ra) # 80001dc4 <_ZN7SuperOS6kalloc18handle_alloc_error17hc1197898e00c3e6fE>

80001e30 <__rust_alloc_error_handler>:
80001e30:	00000317          	auipc	t1,0x0
80001e34:	fec30067          	jr	-20(t1) # 80001e1c <__rg_oom>

80001e38 <_ZN5alloc5alloc18handle_alloc_error17hacdc36dbf7ea50caE>:
80001e38:	ff010113          	addi	sp,sp,-16
80001e3c:	00112623          	sw	ra,12(sp)
80001e40:	00812423          	sw	s0,8(sp)
80001e44:	01010413          	addi	s0,sp,16
80001e48:	00050613          	mv	a2,a0
80001e4c:	00058513          	mv	a0,a1
80001e50:	00060593          	mv	a1,a2
80001e54:	00000097          	auipc	ra,0x0
80001e58:	fdc080e7          	jalr	-36(ra) # 80001e30 <__rust_alloc_error_handler>

80001e5c <_ZN68_$LT$core..ptr..alignment..Alignment$u20$as$u20$core..fmt..Debug$GT$3fmt17h68454b409a0fd924E>:
80001e5c:	fc010113          	addi	sp,sp,-64
80001e60:	02112e23          	sw	ra,60(sp)
80001e64:	02812c23          	sw	s0,56(sp)
80001e68:	04010413          	addi	s0,sp,64
80001e6c:	00052503          	lw	a0,0(a0)
80001e70:	40a00633          	neg	a2,a0
80001e74:	00c57633          	and	a2,a0,a2
80001e78:	077cb6b7          	lui	a3,0x77cb
80001e7c:	53168693          	addi	a3,a3,1329 # 77cb531 <.Lline_table_start2+0x77ca192>
80001e80:	02d60633          	mul	a2,a2,a3
80001e84:	01b65613          	srli	a2,a2,0x1b
80001e88:	800046b7          	lui	a3,0x80004
80001e8c:	09468693          	addi	a3,a3,148 # 80004094 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.1+0x10>
80001e90:	00c68633          	add	a2,a3,a2
80001e94:	00064603          	lbu	a2,0(a2)
80001e98:	fea42823          	sw	a0,-16(s0)
80001e9c:	fec42a23          	sw	a2,-12(s0)
80001ea0:	ff040513          	addi	a0,s0,-16
80001ea4:	fea42023          	sw	a0,-32(s0)
80001ea8:	80003537          	lui	a0,0x80003
80001eac:	53050513          	addi	a0,a0,1328 # 80003530 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E>
80001eb0:	fea42223          	sw	a0,-28(s0)
80001eb4:	ff440513          	addi	a0,s0,-12
80001eb8:	fea42423          	sw	a0,-24(s0)
80001ebc:	80003537          	lui	a0,0x80003
80001ec0:	72850513          	addi	a0,a0,1832 # 80003728 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE>
80001ec4:	fea42623          	sw	a0,-20(s0)
80001ec8:	80005537          	lui	a0,0x80005
80001ecc:	87850513          	addi	a0,a0,-1928 # 80004878 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.140>
80001ed0:	fca42423          	sw	a0,-56(s0)
80001ed4:	00300513          	li	a0,3
80001ed8:	fca42623          	sw	a0,-52(s0)
80001edc:	fc042c23          	sw	zero,-40(s0)
80001ee0:	fe040613          	addi	a2,s0,-32
80001ee4:	0145a503          	lw	a0,20(a1)
80001ee8:	0185a583          	lw	a1,24(a1)
80001eec:	fcc42823          	sw	a2,-48(s0)
80001ef0:	00200613          	li	a2,2
80001ef4:	fcc42a23          	sw	a2,-44(s0)
80001ef8:	fc840613          	addi	a2,s0,-56
80001efc:	00001097          	auipc	ra,0x1
80001f00:	840080e7          	jalr	-1984(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
80001f04:	03c12083          	lw	ra,60(sp)
80001f08:	03812403          	lw	s0,56(sp)
80001f0c:	04010113          	addi	sp,sp,64
80001f10:	00008067          	ret

80001f14 <_ZN4core6option13unwrap_failed17ha917ca27cfe8d772E>:
80001f14:	ff010113          	addi	sp,sp,-16
80001f18:	00112623          	sw	ra,12(sp)
80001f1c:	00812423          	sw	s0,8(sp)
80001f20:	01010413          	addi	s0,sp,16
80001f24:	00050613          	mv	a2,a0
80001f28:	80005537          	lui	a0,0x80005
80001f2c:	89150513          	addi	a0,a0,-1903 # 80004891 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.220>
80001f30:	02b00593          	li	a1,43
80001f34:	00000097          	auipc	ra,0x0
80001f38:	180080e7          	jalr	384(ra) # 800020b4 <_ZN4core9panicking5panic17h651cf8329c8a8911E>

80001f3c <_ZN73_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$3fmt17h69b70629720e2a98E>:
80001f3c:	fb010113          	addi	sp,sp,-80
80001f40:	04112623          	sw	ra,76(sp)
80001f44:	04812423          	sw	s0,72(sp)
80001f48:	04912223          	sw	s1,68(sp)
80001f4c:	05212023          	sw	s2,64(sp)
80001f50:	03312e23          	sw	s3,60(sp)
80001f54:	03412c23          	sw	s4,56(sp)
80001f58:	03512a23          	sw	s5,52(sp)
80001f5c:	05010413          	addi	s0,sp,80
80001f60:	0185a483          	lw	s1,24(a1)
80001f64:	0145a903          	lw	s2,20(a1)
80001f68:	00c4aa83          	lw	s5,12(s1)
80001f6c:	00050993          	mv	s3,a0
80001f70:	800055b7          	lui	a1,0x80005
80001f74:	8d458593          	addi	a1,a1,-1836 # 800048d4 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.222>
80001f78:	00c00613          	li	a2,12
80001f7c:	00090513          	mv	a0,s2
80001f80:	000a80e7          	jalr	s5
80001f84:	00100a13          	li	s4,1
80001f88:	0c051c63          	bnez	a0,80002060 <_ZN73_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$3fmt17h69b70629720e2a98E+0x124>
80001f8c:	0049a503          	lw	a0,4(s3)
80001f90:	00850593          	addi	a1,a0,8
80001f94:	00c50613          	addi	a2,a0,12
80001f98:	fca42623          	sw	a0,-52(s0)
80001f9c:	80004537          	lui	a0,0x80004
80001fa0:	a4050513          	addi	a0,a0,-1472 # 80003a40 <_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h03d1090a3af591adE>
80001fa4:	fca42823          	sw	a0,-48(s0)
80001fa8:	fcb42a23          	sw	a1,-44(s0)
80001fac:	80004537          	lui	a0,0x80004
80001fb0:	82050513          	addi	a0,a0,-2016 # 80003820 <_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17he9e9e363faaccf29E>
80001fb4:	fca42c23          	sw	a0,-40(s0)
80001fb8:	fcc42e23          	sw	a2,-36(s0)
80001fbc:	fea42023          	sw	a0,-32(s0)
80001fc0:	80005537          	lui	a0,0x80005
80001fc4:	8bc50513          	addi	a0,a0,-1860 # 800048bc <.Lanon.0a795d8d80343cc40e42ade3e02d1552.221>
80001fc8:	faa42a23          	sw	a0,-76(s0)
80001fcc:	00300513          	li	a0,3
80001fd0:	faa42c23          	sw	a0,-72(s0)
80001fd4:	fc042223          	sw	zero,-60(s0)
80001fd8:	fcc40593          	addi	a1,s0,-52
80001fdc:	fab42e23          	sw	a1,-68(s0)
80001fe0:	fca42023          	sw	a0,-64(s0)
80001fe4:	fb440613          	addi	a2,s0,-76
80001fe8:	00090513          	mv	a0,s2
80001fec:	00048593          	mv	a1,s1
80001ff0:	00000097          	auipc	ra,0x0
80001ff4:	74c080e7          	jalr	1868(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
80001ff8:	06051463          	bnez	a0,80002060 <_ZN73_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$3fmt17h69b70629720e2a98E+0x124>
80001ffc:	800055b7          	lui	a1,0x80005
80002000:	8e058593          	addi	a1,a1,-1824 # 800048e0 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.223>
80002004:	00200613          	li	a2,2
80002008:	00090513          	mv	a0,s2
8000200c:	000a80e7          	jalr	s5
80002010:	04051863          	bnez	a0,80002060 <_ZN73_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$3fmt17h69b70629720e2a98E+0x124>
80002014:	0009a503          	lw	a0,0(s3)
80002018:	00052583          	lw	a1,0(a0)
8000201c:	fcb42623          	sw	a1,-52(s0)
80002020:	00452583          	lw	a1,4(a0)
80002024:	fcb42823          	sw	a1,-48(s0)
80002028:	00852583          	lw	a1,8(a0)
8000202c:	fcb42a23          	sw	a1,-44(s0)
80002030:	00c52583          	lw	a1,12(a0)
80002034:	fcb42c23          	sw	a1,-40(s0)
80002038:	01052583          	lw	a1,16(a0)
8000203c:	fcb42e23          	sw	a1,-36(s0)
80002040:	01452503          	lw	a0,20(a0)
80002044:	fea42023          	sw	a0,-32(s0)
80002048:	fcc40613          	addi	a2,s0,-52
8000204c:	00090513          	mv	a0,s2
80002050:	00048593          	mv	a1,s1
80002054:	00000097          	auipc	ra,0x0
80002058:	6e8080e7          	jalr	1768(ra) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>
8000205c:	00050a13          	mv	s4,a0
80002060:	000a0513          	mv	a0,s4
80002064:	04c12083          	lw	ra,76(sp)
80002068:	04812403          	lw	s0,72(sp)
8000206c:	04412483          	lw	s1,68(sp)
80002070:	04012903          	lw	s2,64(sp)
80002074:	03c12983          	lw	s3,60(sp)
80002078:	03812a03          	lw	s4,56(sp)
8000207c:	03412a83          	lw	s5,52(sp)
80002080:	05010113          	addi	sp,sp,80
80002084:	00008067          	ret

80002088 <_ZN4core9panicking9panic_fmt17hd44f1c16c40b716eE>:
80002088:	fe010113          	addi	sp,sp,-32
8000208c:	00112e23          	sw	ra,28(sp)
80002090:	00812c23          	sw	s0,24(sp)
80002094:	02010413          	addi	s0,sp,32
80002098:	fea42623          	sw	a0,-20(s0)
8000209c:	feb42823          	sw	a1,-16(s0)
800020a0:	00100513          	li	a0,1
800020a4:	fea41a23          	sh	a0,-12(s0)
800020a8:	fec40513          	addi	a0,s0,-20
800020ac:	fffff097          	auipc	ra,0xfffff
800020b0:	508080e7          	jalr	1288(ra) # 800015b4 <rust_begin_unwind>

800020b4 <_ZN4core9panicking5panic17h651cf8329c8a8911E>:
800020b4:	fd010113          	addi	sp,sp,-48
800020b8:	02112623          	sw	ra,44(sp)
800020bc:	02812423          	sw	s0,40(sp)
800020c0:	03010413          	addi	s0,sp,48
800020c4:	fea42823          	sw	a0,-16(s0)
800020c8:	feb42a23          	sw	a1,-12(s0)
800020cc:	ff040513          	addi	a0,s0,-16
800020d0:	fca42c23          	sw	a0,-40(s0)
800020d4:	00100513          	li	a0,1
800020d8:	fca42e23          	sw	a0,-36(s0)
800020dc:	fe042423          	sw	zero,-24(s0)
800020e0:	00400513          	li	a0,4
800020e4:	fea42023          	sw	a0,-32(s0)
800020e8:	fe042223          	sw	zero,-28(s0)
800020ec:	fd840513          	addi	a0,s0,-40
800020f0:	00060593          	mv	a1,a2
800020f4:	00000097          	auipc	ra,0x0
800020f8:	f94080e7          	jalr	-108(ra) # 80002088 <_ZN4core9panicking9panic_fmt17hd44f1c16c40b716eE>

800020fc <_ZN4core6result13unwrap_failed17h987d8f67a7161eb1E>:
800020fc:	fc010113          	addi	sp,sp,-64
80002100:	02112e23          	sw	ra,60(sp)
80002104:	02812c23          	sw	s0,56(sp)
80002108:	04010413          	addi	s0,sp,64
8000210c:	fca42023          	sw	a0,-64(s0)
80002110:	fcb42223          	sw	a1,-60(s0)
80002114:	fcc42423          	sw	a2,-56(s0)
80002118:	fcd42623          	sw	a3,-52(s0)
8000211c:	fc040513          	addi	a0,s0,-64
80002120:	fea42423          	sw	a0,-24(s0)
80002124:	80004537          	lui	a0,0x80004
80002128:	a4050513          	addi	a0,a0,-1472 # 80003a40 <_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h03d1090a3af591adE>
8000212c:	fea42623          	sw	a0,-20(s0)
80002130:	fc840513          	addi	a0,s0,-56
80002134:	fea42823          	sw	a0,-16(s0)
80002138:	80004537          	lui	a0,0x80004
8000213c:	a1450513          	addi	a0,a0,-1516 # 80003a14 <_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h2421b7a3a4e2e164E>
80002140:	fea42a23          	sw	a0,-12(s0)
80002144:	80005537          	lui	a0,0x80005
80002148:	8e450513          	addi	a0,a0,-1820 # 800048e4 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.251>
8000214c:	fca42823          	sw	a0,-48(s0)
80002150:	00200513          	li	a0,2
80002154:	fca42a23          	sw	a0,-44(s0)
80002158:	fe042023          	sw	zero,-32(s0)
8000215c:	fe840593          	addi	a1,s0,-24
80002160:	fcb42c23          	sw	a1,-40(s0)
80002164:	fca42e23          	sw	a0,-36(s0)
80002168:	fd040513          	addi	a0,s0,-48
8000216c:	00070593          	mv	a1,a4
80002170:	00000097          	auipc	ra,0x0
80002174:	f18080e7          	jalr	-232(ra) # 80002088 <_ZN4core9panicking9panic_fmt17hd44f1c16c40b716eE>

80002178 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E>:
80002178:	fb010113          	addi	sp,sp,-80
8000217c:	04112623          	sw	ra,76(sp)
80002180:	04812423          	sw	s0,72(sp)
80002184:	04912223          	sw	s1,68(sp)
80002188:	05212023          	sw	s2,64(sp)
8000218c:	03312e23          	sw	s3,60(sp)
80002190:	03412c23          	sw	s4,56(sp)
80002194:	03512a23          	sw	s5,52(sp)
80002198:	03612823          	sw	s6,48(sp)
8000219c:	03712623          	sw	s7,44(sp)
800021a0:	03812423          	sw	s8,40(sp)
800021a4:	03912223          	sw	s9,36(sp)
800021a8:	03a12023          	sw	s10,32(sp)
800021ac:	01b12e23          	sw	s11,28(sp)
800021b0:	05010413          	addi	s0,sp,80
800021b4:	00060c13          	mv	s8,a2
800021b8:	00058913          	mv	s2,a1
800021bc:	00000993          	li	s3,0
800021c0:	00000b93          	li	s7,0
800021c4:	00000d13          	li	s10,0
800021c8:	0a0a15b7          	lui	a1,0xa0a1
800021cc:	a0a58a93          	addi	s5,a1,-1526 # a0a0a0a <.Lline_table_start2+0xa09f66b>
800021d0:	010105b7          	lui	a1,0x1010
800021d4:	10058b13          	addi	s6,a1,256 # 1010100 <.Lline_table_start2+0x100ed61>
800021d8:	00852583          	lw	a1,8(a0)
800021dc:	fcb42423          	sw	a1,-56(s0)
800021e0:	00052583          	lw	a1,0(a0)
800021e4:	fcb42223          	sw	a1,-60(s0)
800021e8:	00452503          	lw	a0,4(a0)
800021ec:	fca42023          	sw	a0,-64(s0)
800021f0:	fff90513          	addi	a0,s2,-1
800021f4:	faa42c23          	sw	a0,-72(s0)
800021f8:	40c00533          	neg	a0,a2
800021fc:	faa42e23          	sw	a0,-68(s0)
80002200:	00a00d93          	li	s11,10
80002204:	80808537          	lui	a0,0x80808
80002208:	08050a13          	addi	s4,a0,128 # 80808080 <KALLOC_BUFFER+0x802080>
8000220c:	0400006f          	j	8000224c <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0xd4>
80002210:	fb842503          	lw	a0,-72(s0)
80002214:	00950533          	add	a0,a0,s1
80002218:	00054503          	lbu	a0,0(a0)
8000221c:	ff650513          	addi	a0,a0,-10
80002220:	00153513          	seqz	a0,a0
80002224:	fc842583          	lw	a1,-56(s0)
80002228:	00a58023          	sb	a0,0(a1)
8000222c:	fc042503          	lw	a0,-64(s0)
80002230:	00c52683          	lw	a3,12(a0)
80002234:	41348633          	sub	a2,s1,s3
80002238:	013905b3          	add	a1,s2,s3
8000223c:	fc442503          	lw	a0,-60(s0)
80002240:	000680e7          	jalr	a3
80002244:	000c8993          	mv	s3,s9
80002248:	18051663          	bnez	a0,800023d4 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x25c>
8000224c:	001d7513          	andi	a0,s10,1
80002250:	16051e63          	bnez	a0,800023cc <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x254>
80002254:	057c7863          	bgeu	s8,s7,800022a4 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x12c>
80002258:	00100d13          	li	s10,1
8000225c:	00098c93          	mv	s9,s3
80002260:	000c0493          	mv	s1,s8
80002264:	17898463          	beq	s3,s8,800023cc <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x254>
80002268:	fc842503          	lw	a0,-56(s0)
8000226c:	00054503          	lbu	a0,0(a0)
80002270:	02050263          	beqz	a0,80002294 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x11c>
80002274:	fc042503          	lw	a0,-64(s0)
80002278:	00c52683          	lw	a3,12(a0)
8000227c:	00400613          	li	a2,4
80002280:	fc442503          	lw	a0,-60(s0)
80002284:	800045b7          	lui	a1,0x80004
80002288:	54658593          	addi	a1,a1,1350 # 80004546 <.Lanon.58335f229ba488831bc287488e11d397.7+0x6>
8000228c:	000680e7          	jalr	a3
80002290:	14051263          	bnez	a0,800023d4 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x25c>
80002294:	f7349ee3          	bne	s1,s3,80002210 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x98>
80002298:	00000513          	li	a0,0
8000229c:	f89ff06f          	j	80002224 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0xac>
800022a0:	fb7c6ce3          	bltu	s8,s7,80002258 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0xe0>
800022a4:	417c05b3          	sub	a1,s8,s7
800022a8:	01790533          	add	a0,s2,s7
800022ac:	00700613          	li	a2,7
800022b0:	02b66863          	bltu	a2,a1,800022e0 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x168>
800022b4:	117c0263          	beq	s8,s7,800023b8 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x240>
800022b8:	00000593          	li	a1,0
800022bc:	fbc42603          	lw	a2,-68(s0)
800022c0:	01760633          	add	a2,a2,s7
800022c4:	00050693          	mv	a3,a0
800022c8:	0006c703          	lbu	a4,0(a3)
800022cc:	0bb70c63          	beq	a4,s11,80002384 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x20c>
800022d0:	fff58593          	addi	a1,a1,-1
800022d4:	00168693          	addi	a3,a3,1
800022d8:	feb618e3          	bne	a2,a1,800022c8 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x150>
800022dc:	0dc0006f          	j	800023b8 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x240>
800022e0:	00350713          	addi	a4,a0,3
800022e4:	ffc77713          	andi	a4,a4,-4
800022e8:	40a70633          	sub	a2,a4,a0
800022ec:	02060463          	beqz	a2,80002314 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x19c>
800022f0:	00000693          	li	a3,0
800022f4:	00d507b3          	add	a5,a0,a3
800022f8:	0007c783          	lbu	a5,0(a5)
800022fc:	09b78663          	beq	a5,s11,80002388 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x210>
80002300:	00168693          	addi	a3,a3,1
80002304:	fed618e3          	bne	a2,a3,800022f4 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x17c>
80002308:	ff858693          	addi	a3,a1,-8
8000230c:	00c6f663          	bgeu	a3,a2,80002318 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x1a0>
80002310:	0480006f          	j	80002358 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x1e0>
80002314:	ff858693          	addi	a3,a1,-8
80002318:	00400793          	li	a5,4
8000231c:	00e78733          	add	a4,a5,a4
80002320:	ffc72783          	lw	a5,-4(a4)
80002324:	00072803          	lw	a6,0(a4)
80002328:	0157c8b3          	xor	a7,a5,s5
8000232c:	01584833          	xor	a6,a6,s5
80002330:	410b02b3          	sub	t0,s6,a6
80002334:	0102e833          	or	a6,t0,a6
80002338:	411b08b3          	sub	a7,s6,a7
8000233c:	00f8e7b3          	or	a5,a7,a5
80002340:	0107f7b3          	and	a5,a5,a6
80002344:	0147f7b3          	and	a5,a5,s4
80002348:	01479863          	bne	a5,s4,80002358 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x1e0>
8000234c:	00860613          	addi	a2,a2,8
80002350:	00870713          	addi	a4,a4,8
80002354:	fcc6f6e3          	bgeu	a3,a2,80002320 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x1a8>
80002358:	06b60063          	beq	a2,a1,800023b8 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x240>
8000235c:	00c506b3          	add	a3,a0,a2
80002360:	40c005b3          	neg	a1,a2
80002364:	fbc42603          	lw	a2,-68(s0)
80002368:	01760633          	add	a2,a2,s7
8000236c:	0006c703          	lbu	a4,0(a3)
80002370:	01b70a63          	beq	a4,s11,80002384 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x20c>
80002374:	fff58593          	addi	a1,a1,-1
80002378:	00168693          	addi	a3,a3,1
8000237c:	feb618e3          	bne	a2,a1,8000236c <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x1f4>
80002380:	0380006f          	j	800023b8 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x240>
80002384:	40b006b3          	neg	a3,a1
80002388:	017685b3          	add	a1,a3,s7
8000238c:	00158b93          	addi	s7,a1,1
80002390:	f185f8e3          	bgeu	a1,s8,800022a0 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x128>
80002394:	00d50533          	add	a0,a0,a3
80002398:	00054503          	lbu	a0,0(a0)
8000239c:	f1b512e3          	bne	a0,s11,800022a0 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x128>
800023a0:	000b8c93          	mv	s9,s7
800023a4:	000b8493          	mv	s1,s7
800023a8:	fc842503          	lw	a0,-56(s0)
800023ac:	00054503          	lbu	a0,0(a0)
800023b0:	ee0502e3          	beqz	a0,80002294 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x11c>
800023b4:	ec1ff06f          	j	80002274 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0xfc>
800023b8:	000c0b93          	mv	s7,s8
800023bc:	00100d13          	li	s10,1
800023c0:	00098c93          	mv	s9,s3
800023c4:	000c0493          	mv	s1,s8
800023c8:	eb8990e3          	bne	s3,s8,80002268 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0xf0>
800023cc:	00000513          	li	a0,0
800023d0:	0080006f          	j	800023d8 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E+0x260>
800023d4:	00100513          	li	a0,1
800023d8:	04c12083          	lw	ra,76(sp)
800023dc:	04812403          	lw	s0,72(sp)
800023e0:	04412483          	lw	s1,68(sp)
800023e4:	04012903          	lw	s2,64(sp)
800023e8:	03c12983          	lw	s3,60(sp)
800023ec:	03812a03          	lw	s4,56(sp)
800023f0:	03412a83          	lw	s5,52(sp)
800023f4:	03012b03          	lw	s6,48(sp)
800023f8:	02c12b83          	lw	s7,44(sp)
800023fc:	02812c03          	lw	s8,40(sp)
80002400:	02412c83          	lw	s9,36(sp)
80002404:	02012d03          	lw	s10,32(sp)
80002408:	01c12d83          	lw	s11,28(sp)
8000240c:	05010113          	addi	sp,sp,80
80002410:	00008067          	ret

80002414 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$10write_char17h128366ffe57dce96E>:
80002414:	fe010113          	addi	sp,sp,-32
80002418:	00112e23          	sw	ra,28(sp)
8000241c:	00812c23          	sw	s0,24(sp)
80002420:	00912a23          	sw	s1,20(sp)
80002424:	01212823          	sw	s2,16(sp)
80002428:	01312623          	sw	s3,12(sp)
8000242c:	01412423          	sw	s4,8(sp)
80002430:	02010413          	addi	s0,sp,32
80002434:	00852903          	lw	s2,8(a0)
80002438:	00094603          	lbu	a2,0(s2)
8000243c:	00052483          	lw	s1,0(a0)
80002440:	00452983          	lw	s3,4(a0)
80002444:	04060863          	beqz	a2,80002494 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$10write_char17h128366ffe57dce96E+0x80>
80002448:	00c9a703          	lw	a4,12(s3)
8000244c:	800046b7          	lui	a3,0x80004
80002450:	54668693          	addi	a3,a3,1350 # 80004546 <.Lanon.58335f229ba488831bc287488e11d397.7+0x6>
80002454:	00400613          	li	a2,4
80002458:	00048513          	mv	a0,s1
8000245c:	00058a13          	mv	s4,a1
80002460:	00068593          	mv	a1,a3
80002464:	000700e7          	jalr	a4
80002468:	000a0593          	mv	a1,s4
8000246c:	02050463          	beqz	a0,80002494 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$10write_char17h128366ffe57dce96E+0x80>
80002470:	00100513          	li	a0,1
80002474:	01c12083          	lw	ra,28(sp)
80002478:	01812403          	lw	s0,24(sp)
8000247c:	01412483          	lw	s1,20(sp)
80002480:	01012903          	lw	s2,16(sp)
80002484:	00c12983          	lw	s3,12(sp)
80002488:	00812a03          	lw	s4,8(sp)
8000248c:	02010113          	addi	sp,sp,32
80002490:	00008067          	ret
80002494:	ff658513          	addi	a0,a1,-10
80002498:	00153513          	seqz	a0,a0
8000249c:	00a90023          	sb	a0,0(s2)
800024a0:	0109a303          	lw	t1,16(s3)
800024a4:	00048513          	mv	a0,s1
800024a8:	01c12083          	lw	ra,28(sp)
800024ac:	01812403          	lw	s0,24(sp)
800024b0:	01412483          	lw	s1,20(sp)
800024b4:	01012903          	lw	s2,16(sp)
800024b8:	00c12983          	lw	s3,12(sp)
800024bc:	00812a03          	lw	s4,8(sp)
800024c0:	02010113          	addi	sp,sp,32
800024c4:	00030067          	jr	t1

800024c8 <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E>:
800024c8:	fa010113          	addi	sp,sp,-96
800024cc:	04112e23          	sw	ra,92(sp)
800024d0:	04812c23          	sw	s0,88(sp)
800024d4:	04912a23          	sw	s1,84(sp)
800024d8:	05212823          	sw	s2,80(sp)
800024dc:	05312623          	sw	s3,76(sp)
800024e0:	05412423          	sw	s4,72(sp)
800024e4:	05512223          	sw	s5,68(sp)
800024e8:	05612023          	sw	s6,64(sp)
800024ec:	03712e23          	sw	s7,60(sp)
800024f0:	03812c23          	sw	s8,56(sp)
800024f4:	06010413          	addi	s0,sp,96
800024f8:	00050493          	mv	s1,a0
800024fc:	00454503          	lbu	a0,4(a0)
80002500:	00100b13          	li	s6,1
80002504:	00100a93          	li	s5,1
80002508:	04050063          	beqz	a0,80002548 <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x80>
8000250c:	01548223          	sb	s5,4(s1)
80002510:	016482a3          	sb	s6,5(s1)
80002514:	00048513          	mv	a0,s1
80002518:	05c12083          	lw	ra,92(sp)
8000251c:	05812403          	lw	s0,88(sp)
80002520:	05412483          	lw	s1,84(sp)
80002524:	05012903          	lw	s2,80(sp)
80002528:	04c12983          	lw	s3,76(sp)
8000252c:	04812a03          	lw	s4,72(sp)
80002530:	04412a83          	lw	s5,68(sp)
80002534:	04012b03          	lw	s6,64(sp)
80002538:	03c12b83          	lw	s7,60(sp)
8000253c:	03812c03          	lw	s8,56(sp)
80002540:	06010113          	addi	sp,sp,96
80002544:	00008067          	ret
80002548:	00070993          	mv	s3,a4
8000254c:	00068913          	mv	s2,a3
80002550:	0004aa03          	lw	s4,0(s1)
80002554:	01ca2503          	lw	a0,28(s4)
80002558:	0054c683          	lbu	a3,5(s1)
8000255c:	00457713          	andi	a4,a0,4
80002560:	00071e63          	bnez	a4,8000257c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0xb4>
80002564:	00058b93          	mv	s7,a1
80002568:	00060c13          	mv	s8,a2
8000256c:	10069c63          	bnez	a3,80002684 <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x1bc>
80002570:	800055b7          	lui	a1,0x80005
80002574:	90c58593          	addi	a1,a1,-1780 # 8000490c <.Lanon.0a795d8d80343cc40e42ade3e02d1552.254>
80002578:	1140006f          	j	8000268c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x1c4>
8000257c:	04069063          	bnez	a3,800025bc <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0xf4>
80002580:	018a2683          	lw	a3,24(s4)
80002584:	014a2503          	lw	a0,20(s4)
80002588:	00c6a703          	lw	a4,12(a3)
8000258c:	800056b7          	lui	a3,0x80005
80002590:	91168693          	addi	a3,a3,-1775 # 80004911 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.256>
80002594:	00060a93          	mv	s5,a2
80002598:	00300613          	li	a2,3
8000259c:	00058b93          	mv	s7,a1
800025a0:	00068593          	mv	a1,a3
800025a4:	000700e7          	jalr	a4
800025a8:	000a8613          	mv	a2,s5
800025ac:	00100a93          	li	s5,1
800025b0:	f4051ee3          	bnez	a0,8000250c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x44>
800025b4:	000b8593          	mv	a1,s7
800025b8:	01ca2503          	lw	a0,28(s4)
800025bc:	014a2683          	lw	a3,20(s4)
800025c0:	018a2703          	lw	a4,24(s4)
800025c4:	00100a93          	li	s5,1
800025c8:	fb5409a3          	sb	s5,-77(s0)
800025cc:	fad42223          	sw	a3,-92(s0)
800025d0:	fae42423          	sw	a4,-88(s0)
800025d4:	fb340693          	addi	a3,s0,-77
800025d8:	fad42623          	sw	a3,-84(s0)
800025dc:	010a2683          	lw	a3,16(s4)
800025e0:	020a4703          	lbu	a4,32(s4)
800025e4:	000a2783          	lw	a5,0(s4)
800025e8:	004a2803          	lw	a6,4(s4)
800025ec:	008a2883          	lw	a7,8(s4)
800025f0:	00ca2283          	lw	t0,12(s4)
800025f4:	fca42823          	sw	a0,-48(s0)
800025f8:	fcd42223          	sw	a3,-60(s0)
800025fc:	fce40a23          	sb	a4,-44(s0)
80002600:	faf42a23          	sw	a5,-76(s0)
80002604:	fb042c23          	sw	a6,-72(s0)
80002608:	fb142e23          	sw	a7,-68(s0)
8000260c:	fc542023          	sw	t0,-64(s0)
80002610:	fa440513          	addi	a0,s0,-92
80002614:	fca42423          	sw	a0,-56(s0)
80002618:	80005537          	lui	a0,0x80005
8000261c:	8f450513          	addi	a0,a0,-1804 # 800048f4 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.252>
80002620:	fca42623          	sw	a0,-52(s0)
80002624:	fa440513          	addi	a0,s0,-92
80002628:	00000097          	auipc	ra,0x0
8000262c:	b50080e7          	jalr	-1200(ra) # 80002178 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E>
80002630:	ec051ee3          	bnez	a0,8000250c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x44>
80002634:	800055b7          	lui	a1,0x80005
80002638:	8e258593          	addi	a1,a1,-1822 # 800048e2 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.250>
8000263c:	fa440513          	addi	a0,s0,-92
80002640:	00200613          	li	a2,2
80002644:	00000097          	auipc	ra,0x0
80002648:	b34080e7          	jalr	-1228(ra) # 80002178 <_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hc8bc3946853d1fe4E>
8000264c:	ec0510e3          	bnez	a0,8000250c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x44>
80002650:	00c9a603          	lw	a2,12(s3)
80002654:	fb440593          	addi	a1,s0,-76
80002658:	00090513          	mv	a0,s2
8000265c:	000600e7          	jalr	a2
80002660:	ea0516e3          	bnez	a0,8000250c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x44>
80002664:	fcc42583          	lw	a1,-52(s0)
80002668:	fc842503          	lw	a0,-56(s0)
8000266c:	00c5a683          	lw	a3,12(a1)
80002670:	800055b7          	lui	a1,0x80005
80002674:	91458593          	addi	a1,a1,-1772 # 80004914 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.257>
80002678:	00200613          	li	a2,2
8000267c:	000680e7          	jalr	a3
80002680:	07c0006f          	j	800026fc <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x234>
80002684:	800055b7          	lui	a1,0x80005
80002688:	90f58593          	addi	a1,a1,-1777 # 8000490f <.Lanon.0a795d8d80343cc40e42ade3e02d1552.255>
8000268c:	018a2603          	lw	a2,24(s4)
80002690:	014a2503          	lw	a0,20(s4)
80002694:	00c62703          	lw	a4,12(a2)
80002698:	0036c613          	xori	a2,a3,3
8000269c:	000700e7          	jalr	a4
800026a0:	00100a93          	li	s5,1
800026a4:	e60514e3          	bnez	a0,8000250c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x44>
800026a8:	000c0613          	mv	a2,s8
800026ac:	000b8593          	mv	a1,s7
800026b0:	018a2683          	lw	a3,24(s4)
800026b4:	014a2503          	lw	a0,20(s4)
800026b8:	00c6a683          	lw	a3,12(a3)
800026bc:	000680e7          	jalr	a3
800026c0:	00100a93          	li	s5,1
800026c4:	e40514e3          	bnez	a0,8000250c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x44>
800026c8:	018a2583          	lw	a1,24(s4)
800026cc:	014a2503          	lw	a0,20(s4)
800026d0:	00c5a683          	lw	a3,12(a1)
800026d4:	800055b7          	lui	a1,0x80005
800026d8:	8e258593          	addi	a1,a1,-1822 # 800048e2 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.250>
800026dc:	00200613          	li	a2,2
800026e0:	000680e7          	jalr	a3
800026e4:	00100a93          	li	s5,1
800026e8:	e20512e3          	bnez	a0,8000250c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x44>
800026ec:	00c9a603          	lw	a2,12(s3)
800026f0:	00090513          	mv	a0,s2
800026f4:	000a0593          	mv	a1,s4
800026f8:	000600e7          	jalr	a2
800026fc:	00050a93          	mv	s5,a0
80002700:	e0dff06f          	j	8000250c <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E+0x44>

80002704 <_ZN4core3fmt5Write9write_fmt17h9b1e1bd59d19e928E>:
80002704:	ff010113          	addi	sp,sp,-16
80002708:	00112623          	sw	ra,12(sp)
8000270c:	00812423          	sw	s0,8(sp)
80002710:	01010413          	addi	s0,sp,16
80002714:	80005637          	lui	a2,0x80005
80002718:	8f460613          	addi	a2,a2,-1804 # 800048f4 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.252>
8000271c:	00058693          	mv	a3,a1
80002720:	00060593          	mv	a1,a2
80002724:	00068613          	mv	a2,a3
80002728:	00c12083          	lw	ra,12(sp)
8000272c:	00812403          	lw	s0,8(sp)
80002730:	01010113          	addi	sp,sp,16
80002734:	00000317          	auipc	t1,0x0
80002738:	00830067          	jr	8(t1) # 8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>

8000273c <_ZN4core3fmt5write17h10a3cb6eb3728939E>:
8000273c:	fb010113          	addi	sp,sp,-80
80002740:	04112623          	sw	ra,76(sp)
80002744:	04812423          	sw	s0,72(sp)
80002748:	04912223          	sw	s1,68(sp)
8000274c:	05212023          	sw	s2,64(sp)
80002750:	03312e23          	sw	s3,60(sp)
80002754:	03412c23          	sw	s4,56(sp)
80002758:	03512a23          	sw	s5,52(sp)
8000275c:	03612823          	sw	s6,48(sp)
80002760:	03712623          	sw	s7,44(sp)
80002764:	03812423          	sw	s8,40(sp)
80002768:	05010413          	addi	s0,sp,80
8000276c:	00060493          	mv	s1,a2
80002770:	fc042823          	sw	zero,-48(s0)
80002774:	02000613          	li	a2,32
80002778:	fcc42223          	sw	a2,-60(s0)
8000277c:	00300613          	li	a2,3
80002780:	fcc40a23          	sb	a2,-44(s0)
80002784:	0104ab03          	lw	s6,16(s1)
80002788:	fa042a23          	sw	zero,-76(s0)
8000278c:	fa042e23          	sw	zero,-68(s0)
80002790:	fca42423          	sw	a0,-56(s0)
80002794:	fcb42623          	sw	a1,-52(s0)
80002798:	120b0063          	beqz	s6,800028b8 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x17c>
8000279c:	0144aa83          	lw	s5,20(s1)
800027a0:	180a8863          	beqz	s5,80002930 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x1f4>
800027a4:	0004aa03          	lw	s4,0(s1)
800027a8:	0084a983          	lw	s3,8(s1)
800027ac:	fffa8513          	addi	a0,s5,-1
800027b0:	00551513          	slli	a0,a0,0x5
800027b4:	00555513          	srli	a0,a0,0x5
800027b8:	00150913          	addi	s2,a0,1
800027bc:	004a0a13          	addi	s4,s4,4
800027c0:	005a9a93          	slli	s5,s5,0x5
800027c4:	010b0b13          	addi	s6,s6,16
800027c8:	00200b93          	li	s7,2
800027cc:	00100c13          	li	s8,1
800027d0:	000a2603          	lw	a2,0(s4)
800027d4:	00060e63          	beqz	a2,800027f0 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0xb4>
800027d8:	fcc42683          	lw	a3,-52(s0)
800027dc:	fc842503          	lw	a0,-56(s0)
800027e0:	ffca2583          	lw	a1,-4(s4)
800027e4:	00c6a683          	lw	a3,12(a3)
800027e8:	000680e7          	jalr	a3
800027ec:	16051c63          	bnez	a0,80002964 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x228>
800027f0:	000b2603          	lw	a2,0(s6)
800027f4:	00cb4683          	lbu	a3,12(s6)
800027f8:	008b2703          	lw	a4,8(s6)
800027fc:	ff8b2583          	lw	a1,-8(s6)
80002800:	ffcb2503          	lw	a0,-4(s6)
80002804:	fcc42223          	sw	a2,-60(s0)
80002808:	fcd40a23          	sb	a3,-44(s0)
8000280c:	fce42823          	sw	a4,-48(s0)
80002810:	02058863          	beqz	a1,80002840 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x104>
80002814:	01859a63          	bne	a1,s8,80002828 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0xec>
80002818:	00351513          	slli	a0,a0,0x3
8000281c:	00a98533          	add	a0,s3,a0
80002820:	00052583          	lw	a1,0(a0)
80002824:	00058c63          	beqz	a1,8000283c <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x100>
80002828:	ff0b2603          	lw	a2,-16(s6)
8000282c:	fa042a23          	sw	zero,-76(s0)
80002830:	faa42c23          	sw	a0,-72(s0)
80002834:	03761063          	bne	a2,s7,80002854 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x118>
80002838:	0340006f          	j	8000286c <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x130>
8000283c:	00452503          	lw	a0,4(a0)
80002840:	00100593          	li	a1,1
80002844:	ff0b2603          	lw	a2,-16(s6)
80002848:	fab42a23          	sw	a1,-76(s0)
8000284c:	faa42c23          	sw	a0,-72(s0)
80002850:	01760e63          	beq	a2,s7,8000286c <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x130>
80002854:	ff4b2583          	lw	a1,-12(s6)
80002858:	03861063          	bne	a2,s8,80002878 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x13c>
8000285c:	00359513          	slli	a0,a1,0x3
80002860:	00a98533          	add	a0,s3,a0
80002864:	00052583          	lw	a1,0(a0)
80002868:	00058663          	beqz	a1,80002874 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x138>
8000286c:	00000613          	li	a2,0
80002870:	00c0006f          	j	8000287c <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x140>
80002874:	00452583          	lw	a1,4(a0)
80002878:	00100613          	li	a2,1
8000287c:	004b2503          	lw	a0,4(s6)
80002880:	00351513          	slli	a0,a0,0x3
80002884:	00a986b3          	add	a3,s3,a0
80002888:	0006a503          	lw	a0,0(a3)
8000288c:	0046a683          	lw	a3,4(a3)
80002890:	fac42e23          	sw	a2,-68(s0)
80002894:	fcb42023          	sw	a1,-64(s0)
80002898:	fb440593          	addi	a1,s0,-76
8000289c:	000680e7          	jalr	a3
800028a0:	0c051263          	bnez	a0,80002964 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x228>
800028a4:	008a0a13          	addi	s4,s4,8
800028a8:	fe0a8a93          	addi	s5,s5,-32
800028ac:	020b0b13          	addi	s6,s6,32
800028b0:	f20a90e3          	bnez	s5,800027d0 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x94>
800028b4:	0700006f          	j	80002924 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x1e8>
800028b8:	00c4a503          	lw	a0,12(s1)
800028bc:	06050a63          	beqz	a0,80002930 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x1f4>
800028c0:	0084a983          	lw	s3,8(s1)
800028c4:	00351a13          	slli	s4,a0,0x3
800028c8:	01498a33          	add	s4,s3,s4
800028cc:	0004aa83          	lw	s5,0(s1)
800028d0:	fff50513          	addi	a0,a0,-1
800028d4:	00351513          	slli	a0,a0,0x3
800028d8:	00355513          	srli	a0,a0,0x3
800028dc:	00150913          	addi	s2,a0,1
800028e0:	004a8a93          	addi	s5,s5,4
800028e4:	000aa603          	lw	a2,0(s5)
800028e8:	00060e63          	beqz	a2,80002904 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x1c8>
800028ec:	fcc42683          	lw	a3,-52(s0)
800028f0:	fc842503          	lw	a0,-56(s0)
800028f4:	ffcaa583          	lw	a1,-4(s5)
800028f8:	00c6a683          	lw	a3,12(a3)
800028fc:	000680e7          	jalr	a3
80002900:	06051263          	bnez	a0,80002964 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x228>
80002904:	0009a503          	lw	a0,0(s3)
80002908:	0049a603          	lw	a2,4(s3)
8000290c:	fb440593          	addi	a1,s0,-76
80002910:	000600e7          	jalr	a2
80002914:	04051863          	bnez	a0,80002964 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x228>
80002918:	00898993          	addi	s3,s3,8
8000291c:	008a8a93          	addi	s5,s5,8
80002920:	fd4992e3          	bne	s3,s4,800028e4 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x1a8>
80002924:	0044a503          	lw	a0,4(s1)
80002928:	00a96a63          	bltu	s2,a0,8000293c <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x200>
8000292c:	0400006f          	j	8000296c <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x230>
80002930:	00000913          	li	s2,0
80002934:	0044a503          	lw	a0,4(s1)
80002938:	02a07a63          	bgeu	zero,a0,8000296c <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x230>
8000293c:	0004a503          	lw	a0,0(s1)
80002940:	00391913          	slli	s2,s2,0x3
80002944:	01250933          	add	s2,a0,s2
80002948:	fcc42683          	lw	a3,-52(s0)
8000294c:	fc842503          	lw	a0,-56(s0)
80002950:	00092583          	lw	a1,0(s2)
80002954:	00492603          	lw	a2,4(s2)
80002958:	00c6a683          	lw	a3,12(a3)
8000295c:	000680e7          	jalr	a3
80002960:	00050663          	beqz	a0,8000296c <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x230>
80002964:	00100513          	li	a0,1
80002968:	0080006f          	j	80002970 <_ZN4core3fmt5write17h10a3cb6eb3728939E+0x234>
8000296c:	00000513          	li	a0,0
80002970:	04c12083          	lw	ra,76(sp)
80002974:	04812403          	lw	s0,72(sp)
80002978:	04412483          	lw	s1,68(sp)
8000297c:	04012903          	lw	s2,64(sp)
80002980:	03c12983          	lw	s3,60(sp)
80002984:	03812a03          	lw	s4,56(sp)
80002988:	03412a83          	lw	s5,52(sp)
8000298c:	03012b03          	lw	s6,48(sp)
80002990:	02c12b83          	lw	s7,44(sp)
80002994:	02812c03          	lw	s8,40(sp)
80002998:	05010113          	addi	sp,sp,80
8000299c:	00008067          	ret

800029a0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E>:
800029a0:	fc010113          	addi	sp,sp,-64
800029a4:	02112e23          	sw	ra,60(sp)
800029a8:	02812c23          	sw	s0,56(sp)
800029ac:	02912a23          	sw	s1,52(sp)
800029b0:	03212823          	sw	s2,48(sp)
800029b4:	03312623          	sw	s3,44(sp)
800029b8:	03412423          	sw	s4,40(sp)
800029bc:	03512223          	sw	s5,36(sp)
800029c0:	03612023          	sw	s6,32(sp)
800029c4:	01712e23          	sw	s7,28(sp)
800029c8:	01812c23          	sw	s8,24(sp)
800029cc:	01912a23          	sw	s9,20(sp)
800029d0:	01a12823          	sw	s10,16(sp)
800029d4:	01b12623          	sw	s11,12(sp)
800029d8:	04010413          	addi	s0,sp,64
800029dc:	00078493          	mv	s1,a5
800029e0:	00070913          	mv	s2,a4
800029e4:	00068993          	mv	s3,a3
800029e8:	00060a13          	mv	s4,a2
800029ec:	06058263          	beqz	a1,80002a50 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0xb0>
800029f0:	01c52b03          	lw	s6,28(a0)
800029f4:	001b7c13          	andi	s8,s6,1
800029f8:	00110ab7          	lui	s5,0x110
800029fc:	000c0463          	beqz	s8,80002a04 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x64>
80002a00:	02b00a93          	li	s5,43
80002a04:	009c0c33          	add	s8,s8,s1
80002a08:	004b7593          	andi	a1,s6,4
80002a0c:	04058c63          	beqz	a1,80002a64 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0xc4>
80002a10:	01000593          	li	a1,16
80002a14:	06b9f063          	bgeu	s3,a1,80002a74 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0xd4>
80002a18:	00000593          	li	a1,0
80002a1c:	02098263          	beqz	s3,80002a40 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0xa0>
80002a20:	013a0633          	add	a2,s4,s3
80002a24:	000a0693          	mv	a3,s4
80002a28:	00068703          	lb	a4,0(a3)
80002a2c:	fc072713          	slti	a4,a4,-64
80002a30:	00174713          	xori	a4,a4,1
80002a34:	00168693          	addi	a3,a3,1
80002a38:	00e585b3          	add	a1,a1,a4
80002a3c:	fec696e3          	bne	a3,a2,80002a28 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x88>
80002a40:	01858c33          	add	s8,a1,s8
80002a44:	00052583          	lw	a1,0(a0)
80002a48:	06058e63          	beqz	a1,80002ac4 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x124>
80002a4c:	0500006f          	j	80002a9c <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0xfc>
80002a50:	01c52b03          	lw	s6,28(a0)
80002a54:	00148c13          	addi	s8,s1,1
80002a58:	02d00a93          	li	s5,45
80002a5c:	004b7593          	andi	a1,s6,4
80002a60:	fa0598e3          	bnez	a1,80002a10 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x70>
80002a64:	00000a13          	li	s4,0
80002a68:	00052583          	lw	a1,0(a0)
80002a6c:	02059863          	bnez	a1,80002a9c <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0xfc>
80002a70:	0540006f          	j	80002ac4 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x124>
80002a74:	00050b93          	mv	s7,a0
80002a78:	000a0513          	mv	a0,s4
80002a7c:	00098593          	mv	a1,s3
80002a80:	00001097          	auipc	ra,0x1
80002a84:	828080e7          	jalr	-2008(ra) # 800032a8 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E>
80002a88:	00050593          	mv	a1,a0
80002a8c:	000b8513          	mv	a0,s7
80002a90:	01858c33          	add	s8,a1,s8
80002a94:	000ba583          	lw	a1,0(s7)
80002a98:	02058663          	beqz	a1,80002ac4 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x124>
80002a9c:	00452c83          	lw	s9,4(a0)
80002aa0:	039c7263          	bgeu	s8,s9,80002ac4 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x124>
80002aa4:	008b7593          	andi	a1,s6,8
80002aa8:	08059c63          	bnez	a1,80002b40 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x1a0>
80002aac:	02054583          	lbu	a1,32(a0)
80002ab0:	00100613          	li	a2,1
80002ab4:	418c8cb3          	sub	s9,s9,s8
80002ab8:	0eb64c63          	blt	a2,a1,80002bb0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x210>
80002abc:	10058a63          	beqz	a1,80002bd0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x230>
80002ac0:	1080006f          	j	80002bc8 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x228>
80002ac4:	01452b03          	lw	s6,20(a0)
80002ac8:	01852b83          	lw	s7,24(a0)
80002acc:	000b0513          	mv	a0,s6
80002ad0:	000b8593          	mv	a1,s7
80002ad4:	000a8613          	mv	a2,s5
80002ad8:	000a0693          	mv	a3,s4
80002adc:	00098713          	mv	a4,s3
80002ae0:	00000097          	auipc	ra,0x0
80002ae4:	214080e7          	jalr	532(ra) # 80002cf4 <_ZN4core3fmt9Formatter12pad_integral12write_prefix17h79510df2400b15a5E>
80002ae8:	00050593          	mv	a1,a0
80002aec:	00100513          	li	a0,1
80002af0:	10059863          	bnez	a1,80002c00 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x260>
80002af4:	00cba303          	lw	t1,12(s7)
80002af8:	000b0513          	mv	a0,s6
80002afc:	00090593          	mv	a1,s2
80002b00:	00048613          	mv	a2,s1
80002b04:	03c12083          	lw	ra,60(sp)
80002b08:	03812403          	lw	s0,56(sp)
80002b0c:	03412483          	lw	s1,52(sp)
80002b10:	03012903          	lw	s2,48(sp)
80002b14:	02c12983          	lw	s3,44(sp)
80002b18:	02812a03          	lw	s4,40(sp)
80002b1c:	02412a83          	lw	s5,36(sp)
80002b20:	02012b03          	lw	s6,32(sp)
80002b24:	01c12b83          	lw	s7,28(sp)
80002b28:	01812c03          	lw	s8,24(sp)
80002b2c:	01412c83          	lw	s9,20(sp)
80002b30:	01012d03          	lw	s10,16(sp)
80002b34:	00c12d83          	lw	s11,12(sp)
80002b38:	04010113          	addi	sp,sp,64
80002b3c:	00030067          	jr	t1
80002b40:	01052583          	lw	a1,16(a0)
80002b44:	fcb42423          	sw	a1,-56(s0)
80002b48:	03000593          	li	a1,48
80002b4c:	02054d03          	lbu	s10,32(a0)
80002b50:	01452b03          	lw	s6,20(a0)
80002b54:	01852b83          	lw	s7,24(a0)
80002b58:	00b52823          	sw	a1,16(a0)
80002b5c:	00100593          	li	a1,1
80002b60:	00050d93          	mv	s11,a0
80002b64:	02b50023          	sb	a1,32(a0)
80002b68:	000b0513          	mv	a0,s6
80002b6c:	000b8593          	mv	a1,s7
80002b70:	000a8613          	mv	a2,s5
80002b74:	000a0693          	mv	a3,s4
80002b78:	00098713          	mv	a4,s3
80002b7c:	00000097          	auipc	ra,0x0
80002b80:	178080e7          	jalr	376(ra) # 80002cf4 <_ZN4core3fmt9Formatter12pad_integral12write_prefix17h79510df2400b15a5E>
80002b84:	06051c63          	bnez	a0,80002bfc <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x25c>
80002b88:	418c89b3          	sub	s3,s9,s8
80002b8c:	00198993          	addi	s3,s3,1
80002b90:	fff98993          	addi	s3,s3,-1
80002b94:	12098263          	beqz	s3,80002cb8 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x318>
80002b98:	010ba603          	lw	a2,16(s7)
80002b9c:	03000593          	li	a1,48
80002ba0:	000b0513          	mv	a0,s6
80002ba4:	000600e7          	jalr	a2
80002ba8:	fe0504e3          	beqz	a0,80002b90 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x1f0>
80002bac:	0500006f          	j	80002bfc <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x25c>
80002bb0:	00200613          	li	a2,2
80002bb4:	00c59a63          	bne	a1,a2,80002bc8 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x228>
80002bb8:	001cd593          	srli	a1,s9,0x1
80002bbc:	001c8c93          	addi	s9,s9,1
80002bc0:	001cdc93          	srli	s9,s9,0x1
80002bc4:	00c0006f          	j	80002bd0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x230>
80002bc8:	000c8593          	mv	a1,s9
80002bcc:	00000c93          	li	s9,0
80002bd0:	01452b03          	lw	s6,20(a0)
80002bd4:	01852b83          	lw	s7,24(a0)
80002bd8:	01052c03          	lw	s8,16(a0)
80002bdc:	00158d13          	addi	s10,a1,1
80002be0:	fffd0d13          	addi	s10,s10,-1
80002be4:	040d0c63          	beqz	s10,80002c3c <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x29c>
80002be8:	010ba603          	lw	a2,16(s7)
80002bec:	000b0513          	mv	a0,s6
80002bf0:	000c0593          	mv	a1,s8
80002bf4:	000600e7          	jalr	a2
80002bf8:	fe0504e3          	beqz	a0,80002be0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x240>
80002bfc:	00100513          	li	a0,1
80002c00:	03c12083          	lw	ra,60(sp)
80002c04:	03812403          	lw	s0,56(sp)
80002c08:	03412483          	lw	s1,52(sp)
80002c0c:	03012903          	lw	s2,48(sp)
80002c10:	02c12983          	lw	s3,44(sp)
80002c14:	02812a03          	lw	s4,40(sp)
80002c18:	02412a83          	lw	s5,36(sp)
80002c1c:	02012b03          	lw	s6,32(sp)
80002c20:	01c12b83          	lw	s7,28(sp)
80002c24:	01812c03          	lw	s8,24(sp)
80002c28:	01412c83          	lw	s9,20(sp)
80002c2c:	01012d03          	lw	s10,16(sp)
80002c30:	00c12d83          	lw	s11,12(sp)
80002c34:	04010113          	addi	sp,sp,64
80002c38:	00008067          	ret
80002c3c:	000b0513          	mv	a0,s6
80002c40:	000b8593          	mv	a1,s7
80002c44:	000a8613          	mv	a2,s5
80002c48:	000a0693          	mv	a3,s4
80002c4c:	00098713          	mv	a4,s3
80002c50:	00000097          	auipc	ra,0x0
80002c54:	0a4080e7          	jalr	164(ra) # 80002cf4 <_ZN4core3fmt9Formatter12pad_integral12write_prefix17h79510df2400b15a5E>
80002c58:	00050593          	mv	a1,a0
80002c5c:	00100513          	li	a0,1
80002c60:	fa0590e3          	bnez	a1,80002c00 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x260>
80002c64:	00cba683          	lw	a3,12(s7)
80002c68:	000b0513          	mv	a0,s6
80002c6c:	00090593          	mv	a1,s2
80002c70:	00048613          	mv	a2,s1
80002c74:	000680e7          	jalr	a3
80002c78:	00050593          	mv	a1,a0
80002c7c:	00100513          	li	a0,1
80002c80:	f80590e3          	bnez	a1,80002c00 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x260>
80002c84:	41900933          	neg	s2,s9
80002c88:	fff00993          	li	s3,-1
80002c8c:	fff00493          	li	s1,-1
80002c90:	00990533          	add	a0,s2,s1
80002c94:	05350c63          	beq	a0,s3,80002cec <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x34c>
80002c98:	010ba603          	lw	a2,16(s7)
80002c9c:	000b0513          	mv	a0,s6
80002ca0:	000c0593          	mv	a1,s8
80002ca4:	000600e7          	jalr	a2
80002ca8:	00148493          	addi	s1,s1,1
80002cac:	fe0502e3          	beqz	a0,80002c90 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x2f0>
80002cb0:	0194b533          	sltu	a0,s1,s9
80002cb4:	f4dff06f          	j	80002c00 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x260>
80002cb8:	00cba683          	lw	a3,12(s7)
80002cbc:	000b0513          	mv	a0,s6
80002cc0:	00090593          	mv	a1,s2
80002cc4:	00048613          	mv	a2,s1
80002cc8:	000680e7          	jalr	a3
80002ccc:	00050593          	mv	a1,a0
80002cd0:	00100513          	li	a0,1
80002cd4:	f20596e3          	bnez	a1,80002c00 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x260>
80002cd8:	00000513          	li	a0,0
80002cdc:	fc842583          	lw	a1,-56(s0)
80002ce0:	00bda823          	sw	a1,16(s11)
80002ce4:	03ad8023          	sb	s10,32(s11)
80002ce8:	f19ff06f          	j	80002c00 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x260>
80002cec:	019cb533          	sltu	a0,s9,s9
80002cf0:	f11ff06f          	j	80002c00 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E+0x260>

80002cf4 <_ZN4core3fmt9Formatter12pad_integral12write_prefix17h79510df2400b15a5E>:
80002cf4:	fe010113          	addi	sp,sp,-32
80002cf8:	00112e23          	sw	ra,28(sp)
80002cfc:	00812c23          	sw	s0,24(sp)
80002d00:	00912a23          	sw	s1,20(sp)
80002d04:	01212823          	sw	s2,16(sp)
80002d08:	01312623          	sw	s3,12(sp)
80002d0c:	01412423          	sw	s4,8(sp)
80002d10:	02010413          	addi	s0,sp,32
80002d14:	001107b7          	lui	a5,0x110
80002d18:	00070493          	mv	s1,a4
80002d1c:	00068913          	mv	s2,a3
80002d20:	00058993          	mv	s3,a1
80002d24:	02f60263          	beq	a2,a5,80002d48 <_ZN4core3fmt9Formatter12pad_integral12write_prefix17h79510df2400b15a5E+0x54>
80002d28:	0109a683          	lw	a3,16(s3)
80002d2c:	00050a13          	mv	s4,a0
80002d30:	00060593          	mv	a1,a2
80002d34:	000680e7          	jalr	a3
80002d38:	00050613          	mv	a2,a0
80002d3c:	000a0513          	mv	a0,s4
80002d40:	00100593          	li	a1,1
80002d44:	02061c63          	bnez	a2,80002d7c <_ZN4core3fmt9Formatter12pad_integral12write_prefix17h79510df2400b15a5E+0x88>
80002d48:	02090863          	beqz	s2,80002d78 <_ZN4core3fmt9Formatter12pad_integral12write_prefix17h79510df2400b15a5E+0x84>
80002d4c:	00c9a303          	lw	t1,12(s3)
80002d50:	00090593          	mv	a1,s2
80002d54:	00048613          	mv	a2,s1
80002d58:	01c12083          	lw	ra,28(sp)
80002d5c:	01812403          	lw	s0,24(sp)
80002d60:	01412483          	lw	s1,20(sp)
80002d64:	01012903          	lw	s2,16(sp)
80002d68:	00c12983          	lw	s3,12(sp)
80002d6c:	00812a03          	lw	s4,8(sp)
80002d70:	02010113          	addi	sp,sp,32
80002d74:	00030067          	jr	t1
80002d78:	00000593          	li	a1,0
80002d7c:	00058513          	mv	a0,a1
80002d80:	01c12083          	lw	ra,28(sp)
80002d84:	01812403          	lw	s0,24(sp)
80002d88:	01412483          	lw	s1,20(sp)
80002d8c:	01012903          	lw	s2,16(sp)
80002d90:	00c12983          	lw	s3,12(sp)
80002d94:	00812a03          	lw	s4,8(sp)
80002d98:	02010113          	addi	sp,sp,32
80002d9c:	00008067          	ret

80002da0 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E>:
80002da0:	fd010113          	addi	sp,sp,-48
80002da4:	02112623          	sw	ra,44(sp)
80002da8:	02812423          	sw	s0,40(sp)
80002dac:	02912223          	sw	s1,36(sp)
80002db0:	03212023          	sw	s2,32(sp)
80002db4:	01312e23          	sw	s3,28(sp)
80002db8:	01412c23          	sw	s4,24(sp)
80002dbc:	01512a23          	sw	s5,20(sp)
80002dc0:	01612823          	sw	s6,16(sp)
80002dc4:	01712623          	sw	s7,12(sp)
80002dc8:	03010413          	addi	s0,sp,48
80002dcc:	00052683          	lw	a3,0(a0)
80002dd0:	00852703          	lw	a4,8(a0)
80002dd4:	00060493          	mv	s1,a2
80002dd8:	00058913          	mv	s2,a1
80002ddc:	00177593          	andi	a1,a4,1
80002de0:	00069463          	bnez	a3,80002de8 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x48>
80002de4:	14058463          	beqz	a1,80002f2c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x18c>
80002de8:	0c058263          	beqz	a1,80002eac <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x10c>
80002dec:	00c52703          	lw	a4,12(a0)
80002df0:	00990633          	add	a2,s2,s1
80002df4:	00000593          	li	a1,0
80002df8:	04070e63          	beqz	a4,80002e54 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xb4>
80002dfc:	0e000793          	li	a5,224
80002e00:	0f000813          	li	a6,240
80002e04:	00090893          	mv	a7,s2
80002e08:	01c0006f          	j	80002e24 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x84>
80002e0c:	00188293          	addi	t0,a7,1
80002e10:	40b885b3          	sub	a1,a7,a1
80002e14:	fff70713          	addi	a4,a4,-1
80002e18:	40b285b3          	sub	a1,t0,a1
80002e1c:	00028893          	mv	a7,t0
80002e20:	02070c63          	beqz	a4,80002e58 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xb8>
80002e24:	08c88463          	beq	a7,a2,80002eac <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x10c>
80002e28:	00088283          	lb	t0,0(a7)
80002e2c:	fe02d0e3          	bgez	t0,80002e0c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x6c>
80002e30:	0ff2f293          	zext.b	t0,t0
80002e34:	00f2e863          	bltu	t0,a5,80002e44 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xa4>
80002e38:	0102ea63          	bltu	t0,a6,80002e4c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xac>
80002e3c:	00488293          	addi	t0,a7,4
80002e40:	fd1ff06f          	j	80002e10 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x70>
80002e44:	00288293          	addi	t0,a7,2
80002e48:	fc9ff06f          	j	80002e10 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x70>
80002e4c:	00388293          	addi	t0,a7,3
80002e50:	fc1ff06f          	j	80002e10 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x70>
80002e54:	00090293          	mv	t0,s2
80002e58:	04c28a63          	beq	t0,a2,80002eac <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x10c>
80002e5c:	00028603          	lb	a2,0(t0)
80002e60:	00064663          	bltz	a2,80002e6c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xcc>
80002e64:	00059a63          	bnez	a1,80002e78 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xd8>
80002e68:	0340006f          	j	80002e9c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xfc>
80002e6c:	0ff67613          	zext.b	a2,a2
80002e70:	0e000713          	li	a4,224
80002e74:	02058463          	beqz	a1,80002e9c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xfc>
80002e78:	0295f063          	bgeu	a1,s1,80002e98 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xf8>
80002e7c:	00b90633          	add	a2,s2,a1
80002e80:	00060603          	lb	a2,0(a2)
80002e84:	fc000713          	li	a4,-64
80002e88:	00e65a63          	bge	a2,a4,80002e9c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xfc>
80002e8c:	00000613          	li	a2,0
80002e90:	00001a63          	bnez	zero,80002ea4 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x104>
80002e94:	0180006f          	j	80002eac <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x10c>
80002e98:	fe959ae3          	bne	a1,s1,80002e8c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0xec>
80002e9c:	00090613          	mv	a2,s2
80002ea0:	00090663          	beqz	s2,80002eac <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x10c>
80002ea4:	00058493          	mv	s1,a1
80002ea8:	00060913          	mv	s2,a2
80002eac:	08068063          	beqz	a3,80002f2c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x18c>
80002eb0:	00452983          	lw	s3,4(a0)
80002eb4:	01000593          	li	a1,16
80002eb8:	04b4fa63          	bgeu	s1,a1,80002f0c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x16c>
80002ebc:	00000593          	li	a1,0
80002ec0:	02048263          	beqz	s1,80002ee4 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x144>
80002ec4:	00990633          	add	a2,s2,s1
80002ec8:	00090693          	mv	a3,s2
80002ecc:	00068703          	lb	a4,0(a3)
80002ed0:	fc072713          	slti	a4,a4,-64
80002ed4:	00174713          	xori	a4,a4,1
80002ed8:	00168693          	addi	a3,a3,1
80002edc:	00e585b3          	add	a1,a1,a4
80002ee0:	fec696e3          	bne	a3,a2,80002ecc <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x12c>
80002ee4:	0535f463          	bgeu	a1,s3,80002f2c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x18c>
80002ee8:	02054683          	lbu	a3,32(a0)
80002eec:	00000613          	li	a2,0
80002ef0:	00100713          	li	a4,1
80002ef4:	40b98ab3          	sub	s5,s3,a1
80002ef8:	06d74a63          	blt	a4,a3,80002f6c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x1cc>
80002efc:	08068263          	beqz	a3,80002f80 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x1e0>
80002f00:	000a8613          	mv	a2,s5
80002f04:	00000a93          	li	s5,0
80002f08:	0780006f          	j	80002f80 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x1e0>
80002f0c:	00050a13          	mv	s4,a0
80002f10:	00090513          	mv	a0,s2
80002f14:	00048593          	mv	a1,s1
80002f18:	00000097          	auipc	ra,0x0
80002f1c:	390080e7          	jalr	912(ra) # 800032a8 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E>
80002f20:	00050593          	mv	a1,a0
80002f24:	000a0513          	mv	a0,s4
80002f28:	fd35e0e3          	bltu	a1,s3,80002ee8 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x148>
80002f2c:	01852583          	lw	a1,24(a0)
80002f30:	01452503          	lw	a0,20(a0)
80002f34:	00c5a303          	lw	t1,12(a1)
80002f38:	00090593          	mv	a1,s2
80002f3c:	00048613          	mv	a2,s1
80002f40:	02c12083          	lw	ra,44(sp)
80002f44:	02812403          	lw	s0,40(sp)
80002f48:	02412483          	lw	s1,36(sp)
80002f4c:	02012903          	lw	s2,32(sp)
80002f50:	01c12983          	lw	s3,28(sp)
80002f54:	01812a03          	lw	s4,24(sp)
80002f58:	01412a83          	lw	s5,20(sp)
80002f5c:	01012b03          	lw	s6,16(sp)
80002f60:	00c12b83          	lw	s7,12(sp)
80002f64:	03010113          	addi	sp,sp,48
80002f68:	00030067          	jr	t1
80002f6c:	00200593          	li	a1,2
80002f70:	00b69863          	bne	a3,a1,80002f80 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x1e0>
80002f74:	001ad613          	srli	a2,s5,0x1
80002f78:	001a8a93          	addi	s5,s5,1 # 110001 <.Lline_table_start2+0x10ec62>
80002f7c:	001ada93          	srli	s5,s5,0x1
80002f80:	01452983          	lw	s3,20(a0)
80002f84:	01852b03          	lw	s6,24(a0)
80002f88:	01052a03          	lw	s4,16(a0)
80002f8c:	00160b93          	addi	s7,a2,1
80002f90:	fffb8b93          	addi	s7,s7,-1
80002f94:	020b8063          	beqz	s7,80002fb4 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x214>
80002f98:	010b2603          	lw	a2,16(s6)
80002f9c:	00098513          	mv	a0,s3
80002fa0:	000a0593          	mv	a1,s4
80002fa4:	000600e7          	jalr	a2
80002fa8:	fe0504e3          	beqz	a0,80002f90 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x1f0>
80002fac:	00100513          	li	a0,1
80002fb0:	05c0006f          	j	8000300c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x26c>
80002fb4:	00cb2683          	lw	a3,12(s6)
80002fb8:	00098513          	mv	a0,s3
80002fbc:	00090593          	mv	a1,s2
80002fc0:	00048613          	mv	a2,s1
80002fc4:	000680e7          	jalr	a3
80002fc8:	00050593          	mv	a1,a0
80002fcc:	00100513          	li	a0,1
80002fd0:	02059e63          	bnez	a1,8000300c <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x26c>
80002fd4:	41500933          	neg	s2,s5
80002fd8:	fff00b93          	li	s7,-1
80002fdc:	fff00493          	li	s1,-1
80002fe0:	00990533          	add	a0,s2,s1
80002fe4:	03750063          	beq	a0,s7,80003004 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x264>
80002fe8:	010b2603          	lw	a2,16(s6)
80002fec:	00098513          	mv	a0,s3
80002ff0:	000a0593          	mv	a1,s4
80002ff4:	000600e7          	jalr	a2
80002ff8:	00148493          	addi	s1,s1,1
80002ffc:	fe0502e3          	beqz	a0,80002fe0 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x240>
80003000:	0080006f          	j	80003008 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E+0x268>
80003004:	000a8493          	mv	s1,s5
80003008:	0154b533          	sltu	a0,s1,s5
8000300c:	02c12083          	lw	ra,44(sp)
80003010:	02812403          	lw	s0,40(sp)
80003014:	02412483          	lw	s1,36(sp)
80003018:	02012903          	lw	s2,32(sp)
8000301c:	01c12983          	lw	s3,28(sp)
80003020:	01812a03          	lw	s4,24(sp)
80003024:	01412a83          	lw	s5,20(sp)
80003028:	01012b03          	lw	s6,16(sp)
8000302c:	00c12b83          	lw	s7,12(sp)
80003030:	03010113          	addi	sp,sp,48
80003034:	00008067          	ret

80003038 <_ZN4core3fmt9Formatter9write_str17hd607abcbb12fb4c8E>:
80003038:	ff010113          	addi	sp,sp,-16
8000303c:	00112623          	sw	ra,12(sp)
80003040:	00812423          	sw	s0,8(sp)
80003044:	01010413          	addi	s0,sp,16
80003048:	01852683          	lw	a3,24(a0)
8000304c:	01452503          	lw	a0,20(a0)
80003050:	00c6a303          	lw	t1,12(a3)
80003054:	00c12083          	lw	ra,12(sp)
80003058:	00812403          	lw	s0,8(sp)
8000305c:	01010113          	addi	sp,sp,16
80003060:	00030067          	jr	t1

80003064 <_ZN4core3fmt9Formatter26debug_struct_field2_finish17h8b989ef45de6295cE>:
80003064:	fc010113          	addi	sp,sp,-64
80003068:	02112e23          	sw	ra,60(sp)
8000306c:	02812c23          	sw	s0,56(sp)
80003070:	02912a23          	sw	s1,52(sp)
80003074:	03212823          	sw	s2,48(sp)
80003078:	03312623          	sw	s3,44(sp)
8000307c:	03412423          	sw	s4,40(sp)
80003080:	03512223          	sw	s5,36(sp)
80003084:	03612023          	sw	s6,32(sp)
80003088:	01712e23          	sw	s7,28(sp)
8000308c:	01812c23          	sw	s8,24(sp)
80003090:	01912a23          	sw	s9,20(sp)
80003094:	04010413          	addi	s0,sp,64
80003098:	00050493          	mv	s1,a0
8000309c:	00042903          	lw	s2,0(s0)
800030a0:	01852283          	lw	t0,24(a0)
800030a4:	00442983          	lw	s3,4(s0)
800030a8:	00842a03          	lw	s4,8(s0)
800030ac:	01452503          	lw	a0,20(a0)
800030b0:	00c2a303          	lw	t1,12(t0)
800030b4:	00088a93          	mv	s5,a7
800030b8:	00080b13          	mv	s6,a6
800030bc:	00078b93          	mv	s7,a5
800030c0:	00070c13          	mv	s8,a4
800030c4:	00068c93          	mv	s9,a3
800030c8:	000300e7          	jalr	t1
800030cc:	fc942623          	sw	s1,-52(s0)
800030d0:	fca40823          	sb	a0,-48(s0)
800030d4:	fc0408a3          	sb	zero,-47(s0)
800030d8:	fcc40513          	addi	a0,s0,-52
800030dc:	000c8593          	mv	a1,s9
800030e0:	000c0613          	mv	a2,s8
800030e4:	000b8693          	mv	a3,s7
800030e8:	000b0713          	mv	a4,s6
800030ec:	fffff097          	auipc	ra,0xfffff
800030f0:	3dc080e7          	jalr	988(ra) # 800024c8 <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E>
800030f4:	fcc40513          	addi	a0,s0,-52
800030f8:	000a8593          	mv	a1,s5
800030fc:	00090613          	mv	a2,s2
80003100:	00098693          	mv	a3,s3
80003104:	000a0713          	mv	a4,s4
80003108:	fffff097          	auipc	ra,0xfffff
8000310c:	3c0080e7          	jalr	960(ra) # 800024c8 <_ZN4core3fmt8builders11DebugStruct5field17ha8131389ebaa1b62E>
80003110:	fd144603          	lbu	a2,-47(s0)
80003114:	fd044583          	lbu	a1,-48(s0)
80003118:	00b66533          	or	a0,a2,a1
8000311c:	04060a63          	beqz	a2,80003170 <_ZN4core3fmt9Formatter26debug_struct_field2_finish17h8b989ef45de6295cE+0x10c>
80003120:	0015f593          	andi	a1,a1,1
80003124:	04059663          	bnez	a1,80003170 <_ZN4core3fmt9Formatter26debug_struct_field2_finish17h8b989ef45de6295cE+0x10c>
80003128:	fcc42503          	lw	a0,-52(s0)
8000312c:	01c54583          	lbu	a1,28(a0)
80003130:	0045f593          	andi	a1,a1,4
80003134:	02059063          	bnez	a1,80003154 <_ZN4core3fmt9Formatter26debug_struct_field2_finish17h8b989ef45de6295cE+0xf0>
80003138:	01852583          	lw	a1,24(a0)
8000313c:	01452503          	lw	a0,20(a0)
80003140:	00c5a683          	lw	a3,12(a1)
80003144:	800055b7          	lui	a1,0x80005
80003148:	91758593          	addi	a1,a1,-1769 # 80004917 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.262>
8000314c:	00200613          	li	a2,2
80003150:	01c0006f          	j	8000316c <_ZN4core3fmt9Formatter26debug_struct_field2_finish17h8b989ef45de6295cE+0x108>
80003154:	01852583          	lw	a1,24(a0)
80003158:	01452503          	lw	a0,20(a0)
8000315c:	00c5a683          	lw	a3,12(a1)
80003160:	800055b7          	lui	a1,0x80005
80003164:	91658593          	addi	a1,a1,-1770 # 80004916 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.261>
80003168:	00100613          	li	a2,1
8000316c:	000680e7          	jalr	a3
80003170:	00157513          	andi	a0,a0,1
80003174:	03c12083          	lw	ra,60(sp)
80003178:	03812403          	lw	s0,56(sp)
8000317c:	03412483          	lw	s1,52(sp)
80003180:	03012903          	lw	s2,48(sp)
80003184:	02c12983          	lw	s3,44(sp)
80003188:	02812a03          	lw	s4,40(sp)
8000318c:	02412a83          	lw	s5,36(sp)
80003190:	02012b03          	lw	s6,32(sp)
80003194:	01c12b83          	lw	s7,28(sp)
80003198:	01812c03          	lw	s8,24(sp)
8000319c:	01412c83          	lw	s9,20(sp)
800031a0:	04010113          	addi	sp,sp,64
800031a4:	00008067          	ret

800031a8 <_ZN4core3fmt17pointer_fmt_inner17h3a78f71d335c4ae6E>:
800031a8:	f6010113          	addi	sp,sp,-160
800031ac:	08112e23          	sw	ra,156(sp)
800031b0:	08812c23          	sw	s0,152(sp)
800031b4:	08912a23          	sw	s1,148(sp)
800031b8:	09212823          	sw	s2,144(sp)
800031bc:	09312623          	sw	s3,140(sp)
800031c0:	09412423          	sw	s4,136(sp)
800031c4:	0a010413          	addi	s0,sp,160
800031c8:	00058493          	mv	s1,a1
800031cc:	01c5a903          	lw	s2,28(a1)
800031d0:	0005a983          	lw	s3,0(a1)
800031d4:	0045aa03          	lw	s4,4(a1)
800031d8:	00497613          	andi	a2,s2,4
800031dc:	00090593          	mv	a1,s2
800031e0:	00060e63          	beqz	a2,800031fc <_ZN4core3fmt17pointer_fmt_inner17h3a78f71d335c4ae6E+0x54>
800031e4:	00896593          	ori	a1,s2,8
800031e8:	00099a63          	bnez	s3,800031fc <_ZN4core3fmt17pointer_fmt_inner17h3a78f71d335c4ae6E+0x54>
800031ec:	00100613          	li	a2,1
800031f0:	00c4a023          	sw	a2,0(s1)
800031f4:	00a00613          	li	a2,10
800031f8:	00c4a223          	sw	a2,4(s1)
800031fc:	00000793          	li	a5,0
80003200:	0045e593          	ori	a1,a1,4
80003204:	00b4ae23          	sw	a1,28(s1)
80003208:	fe740593          	addi	a1,s0,-25
8000320c:	00a00613          	li	a2,10
80003210:	01c0006f          	j	8000322c <_ZN4core3fmt17pointer_fmt_inner17h3a78f71d335c4ae6E+0x84>
80003214:	05768693          	addi	a3,a3,87
80003218:	00455513          	srli	a0,a0,0x4
8000321c:	00d58023          	sb	a3,0(a1)
80003220:	00178793          	addi	a5,a5,1 # 110001 <.Lline_table_start2+0x10ec62>
80003224:	fff58593          	addi	a1,a1,-1
80003228:	00050a63          	beqz	a0,8000323c <_ZN4core3fmt17pointer_fmt_inner17h3a78f71d335c4ae6E+0x94>
8000322c:	00f57693          	andi	a3,a0,15
80003230:	fec6f2e3          	bgeu	a3,a2,80003214 <_ZN4core3fmt17pointer_fmt_inner17h3a78f71d335c4ae6E+0x6c>
80003234:	03068693          	addi	a3,a3,48
80003238:	fe1ff06f          	j	80003218 <_ZN4core3fmt17pointer_fmt_inner17h3a78f71d335c4ae6E+0x70>
8000323c:	f6840513          	addi	a0,s0,-152
80003240:	40f50533          	sub	a0,a0,a5
80003244:	08050713          	addi	a4,a0,128
80003248:	80005637          	lui	a2,0x80005
8000324c:	91960613          	addi	a2,a2,-1767 # 80004919 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.289>
80003250:	00100593          	li	a1,1
80003254:	00200693          	li	a3,2
80003258:	00048513          	mv	a0,s1
8000325c:	fffff097          	auipc	ra,0xfffff
80003260:	744080e7          	jalr	1860(ra) # 800029a0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E>
80003264:	0134a023          	sw	s3,0(s1)
80003268:	0144a223          	sw	s4,4(s1)
8000326c:	0124ae23          	sw	s2,28(s1)
80003270:	09c12083          	lw	ra,156(sp)
80003274:	09812403          	lw	s0,152(sp)
80003278:	09412483          	lw	s1,148(sp)
8000327c:	09012903          	lw	s2,144(sp)
80003280:	08c12983          	lw	s3,140(sp)
80003284:	08812a03          	lw	s4,136(sp)
80003288:	0a010113          	addi	sp,sp,160
8000328c:	00008067          	ret

80003290 <_ZN4core5slice5index24slice_end_index_len_fail17h606d05af048992d1E>:
80003290:	ff010113          	addi	sp,sp,-16
80003294:	00112623          	sw	ra,12(sp)
80003298:	00812423          	sw	s0,8(sp)
8000329c:	01010413          	addi	s0,sp,16
800032a0:	00000097          	auipc	ra,0x0
800032a4:	7d4080e7          	jalr	2004(ra) # 80003a74 <_ZN4core5slice5index24slice_end_index_len_fail8do_panic7runtime17h3ad9b894a9807b8aE>

800032a8 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E>:
800032a8:	ff010113          	addi	sp,sp,-16
800032ac:	00112623          	sw	ra,12(sp)
800032b0:	00812423          	sw	s0,8(sp)
800032b4:	01010413          	addi	s0,sp,16
800032b8:	00050613          	mv	a2,a0
800032bc:	00350513          	addi	a0,a0,3
800032c0:	ffc57513          	andi	a0,a0,-4
800032c4:	40c502b3          	sub	t0,a0,a2
800032c8:	0255fc63          	bgeu	a1,t0,80003300 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x58>
800032cc:	00000513          	li	a0,0
800032d0:	02058063          	beqz	a1,800032f0 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x48>
800032d4:	00b605b3          	add	a1,a2,a1
800032d8:	00060683          	lb	a3,0(a2)
800032dc:	fc06a693          	slti	a3,a3,-64
800032e0:	0016c693          	xori	a3,a3,1
800032e4:	00160613          	addi	a2,a2,1
800032e8:	00d50533          	add	a0,a0,a3
800032ec:	feb616e3          	bne	a2,a1,800032d8 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x30>
800032f0:	00c12083          	lw	ra,12(sp)
800032f4:	00812403          	lw	s0,8(sp)
800032f8:	01010113          	addi	sp,sp,16
800032fc:	00008067          	ret
80003300:	405586b3          	sub	a3,a1,t0
80003304:	0026d893          	srli	a7,a3,0x2
80003308:	fc0882e3          	beqz	a7,800032cc <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x24>
8000330c:	005602b3          	add	t0,a2,t0
80003310:	0036f593          	andi	a1,a3,3
80003314:	00c51663          	bne	a0,a2,80003320 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x78>
80003318:	00000513          	li	a0,0
8000331c:	0200006f          	j	8000333c <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x94>
80003320:	00000513          	li	a0,0
80003324:	00060703          	lb	a4,0(a2)
80003328:	fc072713          	slti	a4,a4,-64
8000332c:	00174713          	xori	a4,a4,1
80003330:	00160613          	addi	a2,a2,1
80003334:	00e50533          	add	a0,a0,a4
80003338:	fe5616e3          	bne	a2,t0,80003324 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x7c>
8000333c:	00000713          	li	a4,0
80003340:	02058463          	beqz	a1,80003368 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0xc0>
80003344:	ffc6f613          	andi	a2,a3,-4
80003348:	00c28633          	add	a2,t0,a2
8000334c:	00060683          	lb	a3,0(a2)
80003350:	fc06a693          	slti	a3,a3,-64
80003354:	0016c693          	xori	a3,a3,1
80003358:	00d70733          	add	a4,a4,a3
8000335c:	fff58593          	addi	a1,a1,-1
80003360:	00160613          	addi	a2,a2,1
80003364:	fe0594e3          	bnez	a1,8000334c <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0xa4>
80003368:	010105b7          	lui	a1,0x1010
8000336c:	10158613          	addi	a2,a1,257 # 1010101 <.Lline_table_start2+0x100ed62>
80003370:	00ff05b7          	lui	a1,0xff0
80003374:	0ff58593          	addi	a1,a1,255 # ff00ff <.Lline_table_start2+0xfeed60>
80003378:	00a70533          	add	a0,a4,a0
8000337c:	00400793          	li	a5,4
80003380:	0340006f          	j	800033b4 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x10c>
80003384:	005702b3          	add	t0,a4,t0
80003388:	410688b3          	sub	a7,a3,a6
8000338c:	00387313          	andi	t1,a6,3
80003390:	00b3fe33          	and	t3,t2,a1
80003394:	0083d393          	srli	t2,t2,0x8
80003398:	00b3f3b3          	and	t2,t2,a1
8000339c:	01c383b3          	add	t2,t2,t3
800033a0:	01039e13          	slli	t3,t2,0x10
800033a4:	007e03b3          	add	t2,t3,t2
800033a8:	0103d393          	srli	t2,t2,0x10
800033ac:	00a38533          	add	a0,t2,a0
800033b0:	0a031a63          	bnez	t1,80003464 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x1bc>
800033b4:	f2088ee3          	beqz	a7,800032f0 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x48>
800033b8:	00088693          	mv	a3,a7
800033bc:	00028713          	mv	a4,t0
800033c0:	0c000893          	li	a7,192
800033c4:	00068813          	mv	a6,a3
800033c8:	0116e463          	bltu	a3,a7,800033d0 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x128>
800033cc:	0c000813          	li	a6,192
800033d0:	00281293          	slli	t0,a6,0x2
800033d4:	00000393          	li	t2,0
800033d8:	faf6e6e3          	bltu	a3,a5,80003384 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0xdc>
800033dc:	3f02f893          	andi	a7,t0,1008
800033e0:	011708b3          	add	a7,a4,a7
800033e4:	00070313          	mv	t1,a4
800033e8:	00032e03          	lw	t3,0(t1)
800033ec:	fffe4e93          	not	t4,t3
800033f0:	007ede93          	srli	t4,t4,0x7
800033f4:	006e5e13          	srli	t3,t3,0x6
800033f8:	00432f03          	lw	t5,4(t1)
800033fc:	01ceee33          	or	t3,t4,t3
80003400:	00ce7e33          	and	t3,t3,a2
80003404:	007e03b3          	add	t2,t3,t2
80003408:	ffff4e13          	not	t3,t5
8000340c:	007e5e13          	srli	t3,t3,0x7
80003410:	00832e83          	lw	t4,8(t1)
80003414:	006f5f13          	srli	t5,t5,0x6
80003418:	01ee6e33          	or	t3,t3,t5
8000341c:	00ce7e33          	and	t3,t3,a2
80003420:	fffecf13          	not	t5,t4
80003424:	007f5f13          	srli	t5,t5,0x7
80003428:	006ede93          	srli	t4,t4,0x6
8000342c:	01df6eb3          	or	t4,t5,t4
80003430:	00c32f03          	lw	t5,12(t1)
80003434:	00cefeb3          	and	t4,t4,a2
80003438:	01ce8e33          	add	t3,t4,t3
8000343c:	007e03b3          	add	t2,t3,t2
80003440:	ffff4e13          	not	t3,t5
80003444:	007e5e13          	srli	t3,t3,0x7
80003448:	006f5e93          	srli	t4,t5,0x6
8000344c:	01de6e33          	or	t3,t3,t4
80003450:	00ce7e33          	and	t3,t3,a2
80003454:	01030313          	addi	t1,t1,16
80003458:	007e03b3          	add	t2,t3,t2
8000345c:	f91316e3          	bne	t1,a7,800033e8 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x140>
80003460:	f25ff06f          	j	80003384 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0xdc>
80003464:	00000793          	li	a5,0
80003468:	0fc87813          	andi	a6,a6,252
8000346c:	00281813          	slli	a6,a6,0x2
80003470:	01070733          	add	a4,a4,a6
80003474:	0c06b813          	sltiu	a6,a3,192
80003478:	41000833          	neg	a6,a6
8000347c:	0106f6b3          	and	a3,a3,a6
80003480:	0036f693          	andi	a3,a3,3
80003484:	00269693          	slli	a3,a3,0x2
80003488:	00072803          	lw	a6,0(a4)
8000348c:	00470713          	addi	a4,a4,4
80003490:	fff84893          	not	a7,a6
80003494:	0078d893          	srli	a7,a7,0x7
80003498:	00685813          	srli	a6,a6,0x6
8000349c:	0108e833          	or	a6,a7,a6
800034a0:	00c87833          	and	a6,a6,a2
800034a4:	ffc68693          	addi	a3,a3,-4
800034a8:	00f807b3          	add	a5,a6,a5
800034ac:	fc069ee3          	bnez	a3,80003488 <_ZN4core3str5count14do_count_chars17h4d10955e3b35bc93E+0x1e0>
800034b0:	00b7f633          	and	a2,a5,a1
800034b4:	0087d793          	srli	a5,a5,0x8
800034b8:	00b7f5b3          	and	a1,a5,a1
800034bc:	00c585b3          	add	a1,a1,a2
800034c0:	01059613          	slli	a2,a1,0x10
800034c4:	00b605b3          	add	a1,a2,a1
800034c8:	0105d593          	srli	a1,a1,0x10
800034cc:	00a58533          	add	a0,a1,a0
800034d0:	00c12083          	lw	ra,12(sp)
800034d4:	00812403          	lw	s0,8(sp)
800034d8:	01010113          	addi	sp,sp,16
800034dc:	00008067          	ret

800034e0 <_ZN4core5alloc6layout6Layout19is_size_align_valid17hfcf08246f9a22341E>:
800034e0:	ff010113          	addi	sp,sp,-16
800034e4:	00112623          	sw	ra,12(sp)
800034e8:	00812423          	sw	s0,8(sp)
800034ec:	01010413          	addi	s0,sp,16
800034f0:	fff58613          	addi	a2,a1,-1
800034f4:	00c5c6b3          	xor	a3,a1,a2
800034f8:	02d67263          	bgeu	a2,a3,8000351c <_ZN4core5alloc6layout6Layout19is_size_align_valid17hfcf08246f9a22341E+0x3c>
800034fc:	80000637          	lui	a2,0x80000
80003500:	40b60633          	sub	a2,a2,a1
80003504:	00a63533          	sltu	a0,a2,a0
80003508:	00154513          	xori	a0,a0,1
8000350c:	00c12083          	lw	ra,12(sp)
80003510:	00812403          	lw	s0,8(sp)
80003514:	01010113          	addi	sp,sp,16
80003518:	00008067          	ret
8000351c:	00000513          	li	a0,0
80003520:	00c12083          	lw	ra,12(sp)
80003524:	00812403          	lw	s0,8(sp)
80003528:	01010113          	addi	sp,sp,16
8000352c:	00008067          	ret

80003530 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E>:
80003530:	f7010113          	addi	sp,sp,-144
80003534:	08112623          	sw	ra,140(sp)
80003538:	08812423          	sw	s0,136(sp)
8000353c:	09010413          	addi	s0,sp,144
80003540:	00058813          	mv	a6,a1
80003544:	01c5a583          	lw	a1,28(a1)
80003548:	00052503          	lw	a0,0(a0)
8000354c:	0105f613          	andi	a2,a1,16
80003550:	02061463          	bnez	a2,80003578 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0x48>
80003554:	0205f593          	andi	a1,a1,32
80003558:	04059c63          	bnez	a1,800035b0 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0x80>
8000355c:	00100593          	li	a1,1
80003560:	00080613          	mv	a2,a6
80003564:	08c12083          	lw	ra,140(sp)
80003568:	08812403          	lw	s0,136(sp)
8000356c:	09010113          	addi	sp,sp,144
80003570:	00000317          	auipc	t1,0x0
80003574:	2e030067          	jr	736(t1) # 80003850 <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE>
80003578:	00000793          	li	a5,0
8000357c:	ff740593          	addi	a1,s0,-9
80003580:	00a00613          	li	a2,10
80003584:	01c0006f          	j	800035a0 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0x70>
80003588:	05768693          	addi	a3,a3,87
8000358c:	00455513          	srli	a0,a0,0x4
80003590:	00d58023          	sb	a3,0(a1)
80003594:	00178793          	addi	a5,a5,1
80003598:	fff58593          	addi	a1,a1,-1
8000359c:	04050663          	beqz	a0,800035e8 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0xb8>
800035a0:	00f57693          	andi	a3,a0,15
800035a4:	fec6f2e3          	bgeu	a3,a2,80003588 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0x58>
800035a8:	03068693          	addi	a3,a3,48
800035ac:	fe1ff06f          	j	8000358c <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0x5c>
800035b0:	00000793          	li	a5,0
800035b4:	ff740593          	addi	a1,s0,-9
800035b8:	00a00613          	li	a2,10
800035bc:	01c0006f          	j	800035d8 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0xa8>
800035c0:	03768693          	addi	a3,a3,55
800035c4:	00455513          	srli	a0,a0,0x4
800035c8:	00d58023          	sb	a3,0(a1)
800035cc:	00178793          	addi	a5,a5,1
800035d0:	fff58593          	addi	a1,a1,-1
800035d4:	00050a63          	beqz	a0,800035e8 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0xb8>
800035d8:	00f57693          	andi	a3,a0,15
800035dc:	fec6f2e3          	bgeu	a3,a2,800035c0 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0x90>
800035e0:	03068693          	addi	a3,a3,48
800035e4:	fe1ff06f          	j	800035c4 <_ZN73_$LT$core..num..nonzero..NonZero$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h190a7880edc33533E+0x94>
800035e8:	f7840513          	addi	a0,s0,-136
800035ec:	40f50533          	sub	a0,a0,a5
800035f0:	08050713          	addi	a4,a0,128
800035f4:	80005637          	lui	a2,0x80005
800035f8:	91960613          	addi	a2,a2,-1767 # 80004919 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.289>
800035fc:	00100593          	li	a1,1
80003600:	00200693          	li	a3,2
80003604:	00080513          	mv	a0,a6
80003608:	fffff097          	auipc	ra,0xfffff
8000360c:	398080e7          	jalr	920(ra) # 800029a0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E>
80003610:	08c12083          	lw	ra,140(sp)
80003614:	08812403          	lw	s0,136(sp)
80003618:	09010113          	addi	sp,sp,144
8000361c:	00008067          	ret

80003620 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hcc6fe785c9e9110eE>:
80003620:	f7010113          	addi	sp,sp,-144
80003624:	08112623          	sw	ra,140(sp)
80003628:	08812423          	sw	s0,136(sp)
8000362c:	09010413          	addi	s0,sp,144
80003630:	00052603          	lw	a2,0(a0)
80003634:	00058513          	mv	a0,a1
80003638:	00000793          	li	a5,0
8000363c:	ff740593          	addi	a1,s0,-9
80003640:	00a00693          	li	a3,10
80003644:	01c0006f          	j	80003660 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hcc6fe785c9e9110eE+0x40>
80003648:	05770713          	addi	a4,a4,87
8000364c:	00465613          	srli	a2,a2,0x4
80003650:	00e58023          	sb	a4,0(a1)
80003654:	00178793          	addi	a5,a5,1
80003658:	fff58593          	addi	a1,a1,-1
8000365c:	00060a63          	beqz	a2,80003670 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hcc6fe785c9e9110eE+0x50>
80003660:	00f67713          	andi	a4,a2,15
80003664:	fed772e3          	bgeu	a4,a3,80003648 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hcc6fe785c9e9110eE+0x28>
80003668:	03070713          	addi	a4,a4,48
8000366c:	fe1ff06f          	j	8000364c <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hcc6fe785c9e9110eE+0x2c>
80003670:	f7840593          	addi	a1,s0,-136
80003674:	40f585b3          	sub	a1,a1,a5
80003678:	08058713          	addi	a4,a1,128
8000367c:	80005637          	lui	a2,0x80005
80003680:	91960613          	addi	a2,a2,-1767 # 80004919 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.289>
80003684:	00100593          	li	a1,1
80003688:	00200693          	li	a3,2
8000368c:	fffff097          	auipc	ra,0xfffff
80003690:	314080e7          	jalr	788(ra) # 800029a0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E>
80003694:	08c12083          	lw	ra,140(sp)
80003698:	08812403          	lw	s0,136(sp)
8000369c:	09010113          	addi	sp,sp,144
800036a0:	00008067          	ret

800036a4 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i32$GT$3fmt17h8b16a5bc167e7410E>:
800036a4:	f7010113          	addi	sp,sp,-144
800036a8:	08112623          	sw	ra,140(sp)
800036ac:	08812423          	sw	s0,136(sp)
800036b0:	09010413          	addi	s0,sp,144
800036b4:	00052603          	lw	a2,0(a0)
800036b8:	00058513          	mv	a0,a1
800036bc:	00000793          	li	a5,0
800036c0:	ff740593          	addi	a1,s0,-9
800036c4:	00a00693          	li	a3,10
800036c8:	01c0006f          	j	800036e4 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i32$GT$3fmt17h8b16a5bc167e7410E+0x40>
800036cc:	03770713          	addi	a4,a4,55
800036d0:	00465613          	srli	a2,a2,0x4
800036d4:	00e58023          	sb	a4,0(a1)
800036d8:	00178793          	addi	a5,a5,1
800036dc:	fff58593          	addi	a1,a1,-1
800036e0:	00060a63          	beqz	a2,800036f4 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i32$GT$3fmt17h8b16a5bc167e7410E+0x50>
800036e4:	00f67713          	andi	a4,a2,15
800036e8:	fed772e3          	bgeu	a4,a3,800036cc <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i32$GT$3fmt17h8b16a5bc167e7410E+0x28>
800036ec:	03070713          	addi	a4,a4,48
800036f0:	fe1ff06f          	j	800036d0 <_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i32$GT$3fmt17h8b16a5bc167e7410E+0x2c>
800036f4:	f7840593          	addi	a1,s0,-136
800036f8:	40f585b3          	sub	a1,a1,a5
800036fc:	08058713          	addi	a4,a1,128
80003700:	80005637          	lui	a2,0x80005
80003704:	91960613          	addi	a2,a2,-1767 # 80004919 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.289>
80003708:	00100593          	li	a1,1
8000370c:	00200693          	li	a3,2
80003710:	fffff097          	auipc	ra,0xfffff
80003714:	290080e7          	jalr	656(ra) # 800029a0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E>
80003718:	08c12083          	lw	ra,140(sp)
8000371c:	08812403          	lw	s0,136(sp)
80003720:	09010113          	addi	sp,sp,144
80003724:	00008067          	ret

80003728 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE>:
80003728:	f7010113          	addi	sp,sp,-144
8000372c:	08112623          	sw	ra,140(sp)
80003730:	08812423          	sw	s0,136(sp)
80003734:	09010413          	addi	s0,sp,144
80003738:	00058813          	mv	a6,a1
8000373c:	01c5a583          	lw	a1,28(a1)
80003740:	0105f613          	andi	a2,a1,16
80003744:	02061663          	bnez	a2,80003770 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0x48>
80003748:	0205f593          	andi	a1,a1,32
8000374c:	06059063          	bnez	a1,800037ac <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0x84>
80003750:	00052503          	lw	a0,0(a0)
80003754:	00100593          	li	a1,1
80003758:	00080613          	mv	a2,a6
8000375c:	08c12083          	lw	ra,140(sp)
80003760:	08812403          	lw	s0,136(sp)
80003764:	09010113          	addi	sp,sp,144
80003768:	00000317          	auipc	t1,0x0
8000376c:	0e830067          	jr	232(t1) # 80003850 <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE>
80003770:	00052503          	lw	a0,0(a0)
80003774:	00000793          	li	a5,0
80003778:	ff740593          	addi	a1,s0,-9
8000377c:	00a00613          	li	a2,10
80003780:	01c0006f          	j	8000379c <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0x74>
80003784:	05768693          	addi	a3,a3,87
80003788:	00455513          	srli	a0,a0,0x4
8000378c:	00d58023          	sb	a3,0(a1)
80003790:	00178793          	addi	a5,a5,1
80003794:	fff58593          	addi	a1,a1,-1
80003798:	04050863          	beqz	a0,800037e8 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0xc0>
8000379c:	00f57693          	andi	a3,a0,15
800037a0:	fec6f2e3          	bgeu	a3,a2,80003784 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0x5c>
800037a4:	03068693          	addi	a3,a3,48
800037a8:	fe1ff06f          	j	80003788 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0x60>
800037ac:	00052503          	lw	a0,0(a0)
800037b0:	00000793          	li	a5,0
800037b4:	ff740593          	addi	a1,s0,-9
800037b8:	00a00613          	li	a2,10
800037bc:	01c0006f          	j	800037d8 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0xb0>
800037c0:	03768693          	addi	a3,a3,55
800037c4:	00455513          	srli	a0,a0,0x4
800037c8:	00d58023          	sb	a3,0(a1)
800037cc:	00178793          	addi	a5,a5,1
800037d0:	fff58593          	addi	a1,a1,-1
800037d4:	00050a63          	beqz	a0,800037e8 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0xc0>
800037d8:	00f57693          	andi	a3,a0,15
800037dc:	fec6f2e3          	bgeu	a3,a2,800037c0 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0x98>
800037e0:	03068693          	addi	a3,a3,48
800037e4:	fe1ff06f          	j	800037c4 <_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h7eb5bbc22f53551eE+0x9c>
800037e8:	f7840513          	addi	a0,s0,-136
800037ec:	40f50533          	sub	a0,a0,a5
800037f0:	08050713          	addi	a4,a0,128
800037f4:	80005637          	lui	a2,0x80005
800037f8:	91960613          	addi	a2,a2,-1767 # 80004919 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.289>
800037fc:	00100593          	li	a1,1
80003800:	00200693          	li	a3,2
80003804:	00080513          	mv	a0,a6
80003808:	fffff097          	auipc	ra,0xfffff
8000380c:	198080e7          	jalr	408(ra) # 800029a0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E>
80003810:	08c12083          	lw	ra,140(sp)
80003814:	08812403          	lw	s0,136(sp)
80003818:	09010113          	addi	sp,sp,144
8000381c:	00008067          	ret

80003820 <_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17he9e9e363faaccf29E>:
80003820:	ff010113          	addi	sp,sp,-16
80003824:	00112623          	sw	ra,12(sp)
80003828:	00812423          	sw	s0,8(sp)
8000382c:	01010413          	addi	s0,sp,16
80003830:	00052503          	lw	a0,0(a0)
80003834:	00058613          	mv	a2,a1
80003838:	00100593          	li	a1,1
8000383c:	00c12083          	lw	ra,12(sp)
80003840:	00812403          	lw	s0,8(sp)
80003844:	01010113          	addi	sp,sp,16
80003848:	00000317          	auipc	t1,0x0
8000384c:	00830067          	jr	8(t1) # 80003850 <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE>

80003850 <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE>:
80003850:	fe010113          	addi	sp,sp,-32
80003854:	00112e23          	sw	ra,28(sp)
80003858:	00812c23          	sw	s0,24(sp)
8000385c:	00912a23          	sw	s1,20(sp)
80003860:	01212823          	sw	s2,16(sp)
80003864:	02010413          	addi	s0,sp,32
80003868:	00060693          	mv	a3,a2
8000386c:	00455793          	srli	a5,a0,0x4
80003870:	00a00713          	li	a4,10
80003874:	27100813          	li	a6,625
80003878:	80005637          	lui	a2,0x80005
8000387c:	91b60613          	addi	a2,a2,-1765 # 8000491b <.Lanon.0a795d8d80343cc40e42ade3e02d1552.292>
80003880:	0307f663          	bgeu	a5,a6,800038ac <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE+0x5c>
80003884:	06300793          	li	a5,99
80003888:	0ca7ea63          	bltu	a5,a0,8000395c <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE+0x10c>
8000388c:	00a00793          	li	a5,10
80003890:	12f57263          	bgeu	a0,a5,800039b4 <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE+0x164>
80003894:	fff70793          	addi	a5,a4,-1
80003898:	fe640613          	addi	a2,s0,-26
8000389c:	00f60633          	add	a2,a2,a5
800038a0:	03056513          	ori	a0,a0,48
800038a4:	00a60023          	sb	a0,0(a2)
800038a8:	1300006f          	j	800039d8 <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE+0x188>
800038ac:	00000713          	li	a4,0
800038b0:	fec40793          	addi	a5,s0,-20
800038b4:	fee40813          	addi	a6,s0,-18
800038b8:	d1b718b7          	lui	a7,0xd1b71
800038bc:	75988893          	addi	a7,a7,1881 # d1b71759 <KALLOC_BUFFER+0x51b6b759>
800038c0:	000022b7          	lui	t0,0x2
800038c4:	71028293          	addi	t0,t0,1808 # 2710 <.Lline_table_start2+0x1371>
800038c8:	00001337          	lui	t1,0x1
800038cc:	47b30313          	addi	t1,t1,1147 # 147b <.Lline_table_start2+0xdc>
800038d0:	06400393          	li	t2,100
800038d4:	05f5ee37          	lui	t3,0x5f5e
800038d8:	0ffe0e13          	addi	t3,t3,255 # 5f5e0ff <.Lline_table_start2+0x5f5cd60>
800038dc:	00050e93          	mv	t4,a0
800038e0:	03153533          	mulhu	a0,a0,a7
800038e4:	00d55513          	srli	a0,a0,0xd
800038e8:	02550f33          	mul	t5,a0,t0
800038ec:	41ee8f33          	sub	t5,t4,t5
800038f0:	010f1f93          	slli	t6,t5,0x10
800038f4:	012fdf93          	srli	t6,t6,0x12
800038f8:	026f8fb3          	mul	t6,t6,t1
800038fc:	011fd493          	srli	s1,t6,0x11
80003900:	010fdf93          	srli	t6,t6,0x10
80003904:	7fefff93          	andi	t6,t6,2046
80003908:	027484b3          	mul	s1,s1,t2
8000390c:	409f0f33          	sub	t5,t5,s1
80003910:	011f1f13          	slli	t5,t5,0x11
80003914:	01f60fb3          	add	t6,a2,t6
80003918:	001fc483          	lbu	s1,1(t6)
8000391c:	010f5f13          	srli	t5,t5,0x10
80003920:	00e78933          	add	s2,a5,a4
80003924:	000fcf83          	lbu	t6,0(t6)
80003928:	009900a3          	sb	s1,1(s2)
8000392c:	01e60f33          	add	t5,a2,t5
80003930:	001f4483          	lbu	s1,1(t5)
80003934:	000f4f03          	lbu	t5,0(t5)
80003938:	01f90023          	sb	t6,0(s2)
8000393c:	00e80fb3          	add	t6,a6,a4
80003940:	009f80a3          	sb	s1,1(t6)
80003944:	01ef8023          	sb	t5,0(t6)
80003948:	ffc70713          	addi	a4,a4,-4
8000394c:	f9de68e3          	bltu	t3,t4,800038dc <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE+0x8c>
80003950:	00a70713          	addi	a4,a4,10
80003954:	06300793          	li	a5,99
80003958:	f2a7fae3          	bgeu	a5,a0,8000388c <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE+0x3c>
8000395c:	01051793          	slli	a5,a0,0x10
80003960:	0127d793          	srli	a5,a5,0x12
80003964:	00001837          	lui	a6,0x1
80003968:	47b80813          	addi	a6,a6,1147 # 147b <.Lline_table_start2+0xdc>
8000396c:	030787b3          	mul	a5,a5,a6
80003970:	0117d793          	srli	a5,a5,0x11
80003974:	06400813          	li	a6,100
80003978:	03078833          	mul	a6,a5,a6
8000397c:	41050533          	sub	a0,a0,a6
80003980:	01151513          	slli	a0,a0,0x11
80003984:	01055513          	srli	a0,a0,0x10
80003988:	ffe70713          	addi	a4,a4,-2
8000398c:	00a60533          	add	a0,a2,a0
80003990:	00154803          	lbu	a6,1(a0)
80003994:	00054503          	lbu	a0,0(a0)
80003998:	fe640893          	addi	a7,s0,-26
8000399c:	00e888b3          	add	a7,a7,a4
800039a0:	010880a3          	sb	a6,1(a7)
800039a4:	00a88023          	sb	a0,0(a7)
800039a8:	00078513          	mv	a0,a5
800039ac:	00a00793          	li	a5,10
800039b0:	eef562e3          	bltu	a0,a5,80003894 <_ZN4core3fmt3num3imp21_$LT$impl$u20$u32$GT$4_fmt17hdc3b05787bb19fdaE+0x44>
800039b4:	00151513          	slli	a0,a0,0x1
800039b8:	ffe70793          	addi	a5,a4,-2
800039bc:	00a60533          	add	a0,a2,a0
800039c0:	00154603          	lbu	a2,1(a0)
800039c4:	00054503          	lbu	a0,0(a0)
800039c8:	fe640713          	addi	a4,s0,-26
800039cc:	00f70733          	add	a4,a4,a5
800039d0:	00c700a3          	sb	a2,1(a4)
800039d4:	00a70023          	sb	a0,0(a4)
800039d8:	fe640713          	addi	a4,s0,-26
800039dc:	00f70733          	add	a4,a4,a5
800039e0:	00a00513          	li	a0,10
800039e4:	40f507b3          	sub	a5,a0,a5
800039e8:	00100613          	li	a2,1
800039ec:	00068513          	mv	a0,a3
800039f0:	00000693          	li	a3,0
800039f4:	fffff097          	auipc	ra,0xfffff
800039f8:	fac080e7          	jalr	-84(ra) # 800029a0 <_ZN4core3fmt9Formatter12pad_integral17h031df5456fc12874E>
800039fc:	01c12083          	lw	ra,28(sp)
80003a00:	01812403          	lw	s0,24(sp)
80003a04:	01412483          	lw	s1,20(sp)
80003a08:	01012903          	lw	s2,16(sp)
80003a0c:	02010113          	addi	sp,sp,32
80003a10:	00008067          	ret

80003a14 <_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h2421b7a3a4e2e164E>:
80003a14:	ff010113          	addi	sp,sp,-16
80003a18:	00112623          	sw	ra,12(sp)
80003a1c:	00812423          	sw	s0,8(sp)
80003a20:	01010413          	addi	s0,sp,16
80003a24:	00452603          	lw	a2,4(a0)
80003a28:	00052503          	lw	a0,0(a0)
80003a2c:	00c62303          	lw	t1,12(a2)
80003a30:	00c12083          	lw	ra,12(sp)
80003a34:	00812403          	lw	s0,8(sp)
80003a38:	01010113          	addi	sp,sp,16
80003a3c:	00030067          	jr	t1

80003a40 <_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h03d1090a3af591adE>:
80003a40:	ff010113          	addi	sp,sp,-16
80003a44:	00112623          	sw	ra,12(sp)
80003a48:	00812423          	sw	s0,8(sp)
80003a4c:	01010413          	addi	s0,sp,16
80003a50:	00052683          	lw	a3,0(a0)
80003a54:	00452603          	lw	a2,4(a0)
80003a58:	00058513          	mv	a0,a1
80003a5c:	00068593          	mv	a1,a3
80003a60:	00c12083          	lw	ra,12(sp)
80003a64:	00812403          	lw	s0,8(sp)
80003a68:	01010113          	addi	sp,sp,16
80003a6c:	fffff317          	auipc	t1,0xfffff
80003a70:	33430067          	jr	820(t1) # 80002da0 <_ZN4core3fmt9Formatter3pad17h5337212874eb5ea5E>

80003a74 <_ZN4core5slice5index24slice_end_index_len_fail8do_panic7runtime17h3ad9b894a9807b8aE>:
80003a74:	fc010113          	addi	sp,sp,-64
80003a78:	02112e23          	sw	ra,60(sp)
80003a7c:	02812c23          	sw	s0,56(sp)
80003a80:	04010413          	addi	s0,sp,64
80003a84:	fca42423          	sw	a0,-56(s0)
80003a88:	fcb42623          	sw	a1,-52(s0)
80003a8c:	fc840513          	addi	a0,s0,-56
80003a90:	fea42423          	sw	a0,-24(s0)
80003a94:	80004537          	lui	a0,0x80004
80003a98:	82050513          	addi	a0,a0,-2016 # 80003820 <_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17he9e9e363faaccf29E>
80003a9c:	fea42623          	sw	a0,-20(s0)
80003aa0:	fcc40593          	addi	a1,s0,-52
80003aa4:	feb42823          	sw	a1,-16(s0)
80003aa8:	fea42a23          	sw	a0,-12(s0)
80003aac:	80005537          	lui	a0,0x80005
80003ab0:	a0850513          	addi	a0,a0,-1528 # 80004a08 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.469>
80003ab4:	fca42823          	sw	a0,-48(s0)
80003ab8:	00200513          	li	a0,2
80003abc:	fca42a23          	sw	a0,-44(s0)
80003ac0:	fe042023          	sw	zero,-32(s0)
80003ac4:	fe840593          	addi	a1,s0,-24
80003ac8:	fcb42c23          	sw	a1,-40(s0)
80003acc:	fca42e23          	sw	a0,-36(s0)
80003ad0:	fd040513          	addi	a0,s0,-48
80003ad4:	00060593          	mv	a1,a2
80003ad8:	ffffe097          	auipc	ra,0xffffe
80003adc:	5b0080e7          	jalr	1456(ra) # 80002088 <_ZN4core9panicking9panic_fmt17hd44f1c16c40b716eE>

80003ae0 <memset>:
80003ae0:	ff010113          	addi	sp,sp,-16
80003ae4:	00112623          	sw	ra,12(sp)
80003ae8:	00812423          	sw	s0,8(sp)
80003aec:	01010413          	addi	s0,sp,16
80003af0:	01000693          	li	a3,16
80003af4:	08d66263          	bltu	a2,a3,80003b78 <memset+0x98>
80003af8:	40a006b3          	neg	a3,a0
80003afc:	0036f693          	andi	a3,a3,3
80003b00:	00d50733          	add	a4,a0,a3
80003b04:	00e57e63          	bgeu	a0,a4,80003b20 <memset+0x40>
80003b08:	00068793          	mv	a5,a3
80003b0c:	00050813          	mv	a6,a0
80003b10:	00b80023          	sb	a1,0(a6)
80003b14:	fff78793          	addi	a5,a5,-1
80003b18:	00180813          	addi	a6,a6,1
80003b1c:	fe079ae3          	bnez	a5,80003b10 <memset+0x30>
80003b20:	40d60633          	sub	a2,a2,a3
80003b24:	ffc67693          	andi	a3,a2,-4
80003b28:	00d706b3          	add	a3,a4,a3
80003b2c:	02d77063          	bgeu	a4,a3,80003b4c <memset+0x6c>
80003b30:	0ff5f793          	zext.b	a5,a1
80003b34:	01010837          	lui	a6,0x1010
80003b38:	10180813          	addi	a6,a6,257 # 1010101 <.Lline_table_start2+0x100ed62>
80003b3c:	030787b3          	mul	a5,a5,a6
80003b40:	00f72023          	sw	a5,0(a4)
80003b44:	00470713          	addi	a4,a4,4
80003b48:	fed76ce3          	bltu	a4,a3,80003b40 <memset+0x60>
80003b4c:	00367613          	andi	a2,a2,3
80003b50:	00c68733          	add	a4,a3,a2
80003b54:	00e6fa63          	bgeu	a3,a4,80003b68 <memset+0x88>
80003b58:	00b68023          	sb	a1,0(a3)
80003b5c:	fff60613          	addi	a2,a2,-1
80003b60:	00168693          	addi	a3,a3,1
80003b64:	fe061ae3          	bnez	a2,80003b58 <memset+0x78>
80003b68:	00c12083          	lw	ra,12(sp)
80003b6c:	00812403          	lw	s0,8(sp)
80003b70:	01010113          	addi	sp,sp,16
80003b74:	00008067          	ret
80003b78:	00050693          	mv	a3,a0
80003b7c:	00c50733          	add	a4,a0,a2
80003b80:	fce56ce3          	bltu	a0,a4,80003b58 <memset+0x78>
80003b84:	fe5ff06f          	j	80003b68 <memset+0x88>

80003b88 <memcpy>:
80003b88:	ff010113          	addi	sp,sp,-16
80003b8c:	00112623          	sw	ra,12(sp)
80003b90:	00812423          	sw	s0,8(sp)
80003b94:	01010413          	addi	s0,sp,16
80003b98:	01000693          	li	a3,16
80003b9c:	08d66063          	bltu	a2,a3,80003c1c <memcpy+0x94>
80003ba0:	40a006b3          	neg	a3,a0
80003ba4:	0036f693          	andi	a3,a3,3
80003ba8:	00d50733          	add	a4,a0,a3
80003bac:	02e57463          	bgeu	a0,a4,80003bd4 <memcpy+0x4c>
80003bb0:	00068793          	mv	a5,a3
80003bb4:	00050813          	mv	a6,a0
80003bb8:	00058893          	mv	a7,a1
80003bbc:	0008c283          	lbu	t0,0(a7)
80003bc0:	00580023          	sb	t0,0(a6)
80003bc4:	00180813          	addi	a6,a6,1
80003bc8:	fff78793          	addi	a5,a5,-1
80003bcc:	00188893          	addi	a7,a7,1
80003bd0:	fe0796e3          	bnez	a5,80003bbc <memcpy+0x34>
80003bd4:	00d585b3          	add	a1,a1,a3
80003bd8:	40d60633          	sub	a2,a2,a3
80003bdc:	ffc67793          	andi	a5,a2,-4
80003be0:	0035f813          	andi	a6,a1,3
80003be4:	00f706b3          	add	a3,a4,a5
80003be8:	06081463          	bnez	a6,80003c50 <memcpy+0xc8>
80003bec:	00d77e63          	bgeu	a4,a3,80003c08 <memcpy+0x80>
80003bf0:	00058813          	mv	a6,a1
80003bf4:	00082883          	lw	a7,0(a6)
80003bf8:	01172023          	sw	a7,0(a4)
80003bfc:	00470713          	addi	a4,a4,4
80003c00:	00480813          	addi	a6,a6,4
80003c04:	fed768e3          	bltu	a4,a3,80003bf4 <memcpy+0x6c>
80003c08:	00f585b3          	add	a1,a1,a5
80003c0c:	00367613          	andi	a2,a2,3
80003c10:	00c68733          	add	a4,a3,a2
80003c14:	00e6ea63          	bltu	a3,a4,80003c28 <memcpy+0xa0>
80003c18:	0280006f          	j	80003c40 <memcpy+0xb8>
80003c1c:	00050693          	mv	a3,a0
80003c20:	00c50733          	add	a4,a0,a2
80003c24:	00e57e63          	bgeu	a0,a4,80003c40 <memcpy+0xb8>
80003c28:	0005c703          	lbu	a4,0(a1)
80003c2c:	00e68023          	sb	a4,0(a3)
80003c30:	00168693          	addi	a3,a3,1
80003c34:	fff60613          	addi	a2,a2,-1
80003c38:	00158593          	addi	a1,a1,1
80003c3c:	fe0616e3          	bnez	a2,80003c28 <memcpy+0xa0>
80003c40:	00c12083          	lw	ra,12(sp)
80003c44:	00812403          	lw	s0,8(sp)
80003c48:	01010113          	addi	sp,sp,16
80003c4c:	00008067          	ret
80003c50:	fad77ce3          	bgeu	a4,a3,80003c08 <memcpy+0x80>
80003c54:	00359893          	slli	a7,a1,0x3
80003c58:	0188f813          	andi	a6,a7,24
80003c5c:	ffc5f293          	andi	t0,a1,-4
80003c60:	0002a303          	lw	t1,0(t0)
80003c64:	411008b3          	neg	a7,a7
80003c68:	0188f893          	andi	a7,a7,24
80003c6c:	00428293          	addi	t0,t0,4
80003c70:	0002a383          	lw	t2,0(t0)
80003c74:	01035333          	srl	t1,t1,a6
80003c78:	01139e33          	sll	t3,t2,a7
80003c7c:	006e6333          	or	t1,t3,t1
80003c80:	00672023          	sw	t1,0(a4)
80003c84:	00470713          	addi	a4,a4,4
80003c88:	00428293          	addi	t0,t0,4
80003c8c:	00038313          	mv	t1,t2
80003c90:	fed760e3          	bltu	a4,a3,80003c70 <memcpy+0xe8>
80003c94:	f75ff06f          	j	80003c08 <memcpy+0x80>

80003c98 <memmove>:
80003c98:	ff010113          	addi	sp,sp,-16
80003c9c:	00112623          	sw	ra,12(sp)
80003ca0:	00812423          	sw	s0,8(sp)
80003ca4:	01010413          	addi	s0,sp,16
80003ca8:	00c12083          	lw	ra,12(sp)
80003cac:	00812403          	lw	s0,8(sp)
80003cb0:	01010113          	addi	sp,sp,16
80003cb4:	00000317          	auipc	t1,0x0
80003cb8:	00830067          	jr	8(t1) # 80003cbc <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E>

80003cbc <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E>:
80003cbc:	ff010113          	addi	sp,sp,-16
80003cc0:	00112623          	sw	ra,12(sp)
80003cc4:	00812423          	sw	s0,8(sp)
80003cc8:	01010413          	addi	s0,sp,16
80003ccc:	40b506b3          	sub	a3,a0,a1
80003cd0:	0ac6fa63          	bgeu	a3,a2,80003d84 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0xc8>
80003cd4:	00c506b3          	add	a3,a0,a2
80003cd8:	01000713          	li	a4,16
80003cdc:	00c587b3          	add	a5,a1,a2
80003ce0:	06e66e63          	bltu	a2,a4,80003d5c <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0xa0>
80003ce4:	0036f813          	andi	a6,a3,3
80003ce8:	ffc6f713          	andi	a4,a3,-4
80003cec:	410008b3          	neg	a7,a6
80003cf0:	02d77263          	bgeu	a4,a3,80003d14 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x58>
80003cf4:	00c582b3          	add	t0,a1,a2
80003cf8:	fff28293          	addi	t0,t0,-1
80003cfc:	0002c303          	lbu	t1,0(t0)
80003d00:	fff68393          	addi	t2,a3,-1
80003d04:	fe668fa3          	sb	t1,-1(a3)
80003d08:	fff28293          	addi	t0,t0,-1
80003d0c:	00038693          	mv	a3,t2
80003d10:	fe7766e3          	bltu	a4,t2,80003cfc <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x40>
80003d14:	011787b3          	add	a5,a5,a7
80003d18:	41060633          	sub	a2,a2,a6
80003d1c:	ffc67693          	andi	a3,a2,-4
80003d20:	0037f893          	andi	a7,a5,3
80003d24:	40d00833          	neg	a6,a3
80003d28:	40d706b3          	sub	a3,a4,a3
80003d2c:	10089863          	bnez	a7,80003e3c <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x180>
80003d30:	02e6f263          	bgeu	a3,a4,80003d54 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x98>
80003d34:	00b605b3          	add	a1,a2,a1
80003d38:	ffc58593          	addi	a1,a1,-4
80003d3c:	0005a883          	lw	a7,0(a1)
80003d40:	ffc70293          	addi	t0,a4,-4
80003d44:	ff172e23          	sw	a7,-4(a4)
80003d48:	ffc58593          	addi	a1,a1,-4
80003d4c:	00028713          	mv	a4,t0
80003d50:	fe56e6e3          	bltu	a3,t0,80003d3c <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x80>
80003d54:	010787b3          	add	a5,a5,a6
80003d58:	00367613          	andi	a2,a2,3
80003d5c:	40c685b3          	sub	a1,a3,a2
80003d60:	0cd5f663          	bgeu	a1,a3,80003e2c <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x170>
80003d64:	fff78793          	addi	a5,a5,-1
80003d68:	0007c603          	lbu	a2,0(a5)
80003d6c:	fff68713          	addi	a4,a3,-1
80003d70:	fec68fa3          	sb	a2,-1(a3)
80003d74:	fff78793          	addi	a5,a5,-1
80003d78:	00070693          	mv	a3,a4
80003d7c:	fee5e6e3          	bltu	a1,a4,80003d68 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0xac>
80003d80:	0ac0006f          	j	80003e2c <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x170>
80003d84:	01000693          	li	a3,16
80003d88:	08d66063          	bltu	a2,a3,80003e08 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x14c>
80003d8c:	40a006b3          	neg	a3,a0
80003d90:	0036f693          	andi	a3,a3,3
80003d94:	00d50733          	add	a4,a0,a3
80003d98:	02e57463          	bgeu	a0,a4,80003dc0 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x104>
80003d9c:	00068793          	mv	a5,a3
80003da0:	00050813          	mv	a6,a0
80003da4:	00058893          	mv	a7,a1
80003da8:	0008c283          	lbu	t0,0(a7)
80003dac:	00580023          	sb	t0,0(a6)
80003db0:	00180813          	addi	a6,a6,1
80003db4:	fff78793          	addi	a5,a5,-1
80003db8:	00188893          	addi	a7,a7,1
80003dbc:	fe0796e3          	bnez	a5,80003da8 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0xec>
80003dc0:	00d585b3          	add	a1,a1,a3
80003dc4:	40d60633          	sub	a2,a2,a3
80003dc8:	ffc67793          	andi	a5,a2,-4
80003dcc:	0035f813          	andi	a6,a1,3
80003dd0:	00f706b3          	add	a3,a4,a5
80003dd4:	0a081a63          	bnez	a6,80003e88 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x1cc>
80003dd8:	00d77e63          	bgeu	a4,a3,80003df4 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x138>
80003ddc:	00058813          	mv	a6,a1
80003de0:	00082883          	lw	a7,0(a6)
80003de4:	01172023          	sw	a7,0(a4)
80003de8:	00470713          	addi	a4,a4,4
80003dec:	00480813          	addi	a6,a6,4
80003df0:	fed768e3          	bltu	a4,a3,80003de0 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x124>
80003df4:	00f585b3          	add	a1,a1,a5
80003df8:	00367613          	andi	a2,a2,3
80003dfc:	00c68733          	add	a4,a3,a2
80003e00:	00e6ea63          	bltu	a3,a4,80003e14 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x158>
80003e04:	0280006f          	j	80003e2c <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x170>
80003e08:	00050693          	mv	a3,a0
80003e0c:	00c50733          	add	a4,a0,a2
80003e10:	00e57e63          	bgeu	a0,a4,80003e2c <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x170>
80003e14:	0005c703          	lbu	a4,0(a1)
80003e18:	00e68023          	sb	a4,0(a3)
80003e1c:	00168693          	addi	a3,a3,1
80003e20:	fff60613          	addi	a2,a2,-1
80003e24:	00158593          	addi	a1,a1,1
80003e28:	fe0616e3          	bnez	a2,80003e14 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x158>
80003e2c:	00c12083          	lw	ra,12(sp)
80003e30:	00812403          	lw	s0,8(sp)
80003e34:	01010113          	addi	sp,sp,16
80003e38:	00008067          	ret
80003e3c:	f0e6fce3          	bgeu	a3,a4,80003d54 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x98>
80003e40:	00379893          	slli	a7,a5,0x3
80003e44:	0188f593          	andi	a1,a7,24
80003e48:	ffc7f293          	andi	t0,a5,-4
80003e4c:	0002a303          	lw	t1,0(t0)
80003e50:	411008b3          	neg	a7,a7
80003e54:	0188f893          	andi	a7,a7,24
80003e58:	ffc28293          	addi	t0,t0,-4
80003e5c:	0002a383          	lw	t2,0(t0)
80003e60:	01131333          	sll	t1,t1,a7
80003e64:	00b3de33          	srl	t3,t2,a1
80003e68:	006e6333          	or	t1,t3,t1
80003e6c:	ffc70e13          	addi	t3,a4,-4
80003e70:	fe672e23          	sw	t1,-4(a4)
80003e74:	ffc28293          	addi	t0,t0,-4
80003e78:	000e0713          	mv	a4,t3
80003e7c:	00038313          	mv	t1,t2
80003e80:	fdc6eee3          	bltu	a3,t3,80003e5c <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x1a0>
80003e84:	ed1ff06f          	j	80003d54 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x98>
80003e88:	f6d776e3          	bgeu	a4,a3,80003df4 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x138>
80003e8c:	00359893          	slli	a7,a1,0x3
80003e90:	0188f813          	andi	a6,a7,24
80003e94:	ffc5f293          	andi	t0,a1,-4
80003e98:	0002a303          	lw	t1,0(t0)
80003e9c:	411008b3          	neg	a7,a7
80003ea0:	0188f893          	andi	a7,a7,24
80003ea4:	00428293          	addi	t0,t0,4
80003ea8:	0002a383          	lw	t2,0(t0)
80003eac:	01035333          	srl	t1,t1,a6
80003eb0:	01139e33          	sll	t3,t2,a7
80003eb4:	006e6333          	or	t1,t3,t1
80003eb8:	00672023          	sw	t1,0(a4)
80003ebc:	00470713          	addi	a4,a4,4
80003ec0:	00428293          	addi	t0,t0,4
80003ec4:	00038313          	mv	t1,t2
80003ec8:	fed760e3          	bltu	a4,a3,80003ea8 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x1ec>
80003ecc:	f29ff06f          	j	80003df4 <_ZN17compiler_builtins3mem7memmove17he8ff7b0907bf41e0E+0x138>
	...

Disassembly of section .rodata:

80004000 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.0>:
80004000:	6d6f682f          	.insn	4, 0x6d6f682f
80004004:	2f65                	.insn	2, 0x2f65
80004006:	6572                	.insn	2, 0x6572
80004008:	796d                	.insn	2, 0x796d
8000400a:	75722e2f          	.insn	4, 0x75722e2f
8000400e:	70757473          	.insn	4, 0x70757473
80004012:	6f6f742f          	.insn	4, 0x6f6f742f
80004016:	636c                	.insn	2, 0x636c
80004018:	6168                	.insn	2, 0x6168
8000401a:	6e69                	.insn	2, 0x6e69
8000401c:	696e2f73          	.insn	4, 0x696e2f73
80004020:	6c746867          	.insn	4, 0x6c746867
80004024:	2d79                	.insn	2, 0x2d79
80004026:	3878                	.insn	2, 0x3878
80004028:	5f36                	.insn	2, 0x5f36
8000402a:	3436                	.insn	2, 0x3436
8000402c:	752d                	.insn	2, 0x752d
8000402e:	6b6e                	.insn	2, 0x6b6e
80004030:	6f6e                	.insn	2, 0x6f6e
80004032:	6c2d6e77          	.insn	4, 0x6c2d6e77
80004036:	6e69                	.insn	2, 0x6e69
80004038:	7875                	.insn	2, 0x7875
8000403a:	672d                	.insn	2, 0x672d
8000403c:	756e                	.insn	2, 0x756e
8000403e:	62696c2f          	.insn	4, 0x62696c2f
80004042:	7375722f          	.insn	4, 0x7375722f
80004046:	6c74                	.insn	2, 0x6c74
80004048:	6269                	.insn	2, 0x6269
8000404a:	6372732f          	.insn	4, 0x6372732f
8000404e:	7375722f          	.insn	4, 0x7375722f
80004052:	2f74                	.insn	2, 0x2f74
80004054:	696c                	.insn	2, 0x696c
80004056:	7262                	.insn	2, 0x7262
80004058:	7261                	.insn	2, 0x7261
8000405a:	2f79                	.insn	2, 0x2f79
8000405c:	6c61                	.insn	2, 0x6c61
8000405e:	6f6c                	.insn	2, 0x6f6c
80004060:	72732f63          	.insn	4, 0x72732f63
80004064:	6f632f63          	.insn	4, 0x6f632f63
80004068:	6c6c                	.insn	2, 0x6c6c
8000406a:	6365                	.insn	2, 0x6365
8000406c:	6974                	.insn	2, 0x6974
8000406e:	2f736e6f          	jal	t3,8003ab64 <KALLOC_BUFFER+0x34b64>
80004072:	7462                	.insn	2, 0x7462
80004074:	6572                	.insn	2, 0x6572
80004076:	2f65                	.insn	2, 0x2f65
80004078:	616d                	.insn	2, 0x616d
8000407a:	2f70                	.insn	2, 0x2f70
8000407c:	6e65                	.insn	2, 0x6e65
8000407e:	7274                	.insn	2, 0x7274
80004080:	2e79                	.insn	2, 0x2e79
80004082:	7372                	.insn	2, 0x7372

80004084 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.1>:
80004084:	4000                	.insn	2, 0x4000
80004086:	8000                	.insn	2, 0x8000
80004088:	0084                	.insn	2, 0x0084
8000408a:	0000                	.insn	2, 0x
8000408c:	0171                	.insn	2, 0x0171
8000408e:	0000                	.insn	2, 0x
80004090:	0036                	.insn	2, 0x0036
80004092:	0000                	.insn	2, 0x
80004094:	0100                	.insn	2, 0x0100
80004096:	021c                	.insn	2, 0x021c
80004098:	0e1d                	.insn	2, 0x0e1d
8000409a:	0318                	.insn	2, 0x0318
8000409c:	161e                	.insn	2, 0x161e
8000409e:	0f14                	.insn	2, 0x0f14
800040a0:	1119                	.insn	2, 0x1119
800040a2:	0804                	.insn	2, 0x0804
800040a4:	1b1f 170d 1315      	.insn	6, 0x1315170d1b1f
800040aa:	0710                	.insn	2, 0x0710
800040ac:	0c1a                	.insn	2, 0x0c1a
800040ae:	0612                	.insn	2, 0x0612
800040b0:	090a050b          	.insn	4, 0x090a050b

800040b4 <anon.0967ae7c4fd660b9acdb752d1aeda62f.2.llvm.1023813754811204676>:
800040b4:	7361                	.insn	2, 0x7361
800040b6:	74726573          	.insn	4, 0x74726573
800040ba:	6f69                	.insn	2, 0x6f69
800040bc:	206e                	.insn	2, 0x206e
800040be:	6166                	.insn	2, 0x6166
800040c0:	6c69                	.insn	2, 0x6c69
800040c2:	6465                	.insn	2, 0x6465
800040c4:	203a                	.insn	2, 0x203a
800040c6:	6469                	.insn	2, 0x6469
800040c8:	2078                	.insn	2, 0x2078
800040ca:	203c                	.insn	2, 0x203c
800040cc:	41504143          	.insn	4, 0x41504143
800040d0:	59544943          	.insn	4, 0x59544943

800040d4 <anon.0967ae7c4fd660b9acdb752d1aeda62f.3.llvm.1023813754811204676>:
800040d4:	6d6f682f          	.insn	4, 0x6d6f682f
800040d8:	2f65                	.insn	2, 0x2f65
800040da:	6572                	.insn	2, 0x6572
800040dc:	796d                	.insn	2, 0x796d
800040de:	75722e2f          	.insn	4, 0x75722e2f
800040e2:	70757473          	.insn	4, 0x70757473
800040e6:	6f6f742f          	.insn	4, 0x6f6f742f
800040ea:	636c                	.insn	2, 0x636c
800040ec:	6168                	.insn	2, 0x6168
800040ee:	6e69                	.insn	2, 0x6e69
800040f0:	696e2f73          	.insn	4, 0x696e2f73
800040f4:	6c746867          	.insn	4, 0x6c746867
800040f8:	2d79                	.insn	2, 0x2d79
800040fa:	3878                	.insn	2, 0x3878
800040fc:	5f36                	.insn	2, 0x5f36
800040fe:	3436                	.insn	2, 0x3436
80004100:	752d                	.insn	2, 0x752d
80004102:	6b6e                	.insn	2, 0x6b6e
80004104:	6f6e                	.insn	2, 0x6f6e
80004106:	6c2d6e77          	.insn	4, 0x6c2d6e77
8000410a:	6e69                	.insn	2, 0x6e69
8000410c:	7875                	.insn	2, 0x7875
8000410e:	672d                	.insn	2, 0x672d
80004110:	756e                	.insn	2, 0x756e
80004112:	62696c2f          	.insn	4, 0x62696c2f
80004116:	7375722f          	.insn	4, 0x7375722f
8000411a:	6c74                	.insn	2, 0x6c74
8000411c:	6269                	.insn	2, 0x6269
8000411e:	6372732f          	.insn	4, 0x6372732f
80004122:	7375722f          	.insn	4, 0x7375722f
80004126:	2f74                	.insn	2, 0x2f74
80004128:	696c                	.insn	2, 0x696c
8000412a:	7262                	.insn	2, 0x7262
8000412c:	7261                	.insn	2, 0x7261
8000412e:	2f79                	.insn	2, 0x2f79
80004130:	6c61                	.insn	2, 0x6c61
80004132:	6f6c                	.insn	2, 0x6f6c
80004134:	72732f63          	.insn	4, 0x72732f63
80004138:	6f632f63          	.insn	4, 0x6f632f63
8000413c:	6c6c                	.insn	2, 0x6c6c
8000413e:	6365                	.insn	2, 0x6365
80004140:	6974                	.insn	2, 0x6974
80004142:	2f736e6f          	jal	t3,8003ac38 <KALLOC_BUFFER+0x34c38>
80004146:	7462                	.insn	2, 0x7462
80004148:	6572                	.insn	2, 0x6572
8000414a:	2f65                	.insn	2, 0x2f65
8000414c:	6f6e                	.insn	2, 0x6f6e
8000414e:	6564                	.insn	2, 0x6564
80004150:	722e                	.insn	2, 0x722e
80004152:	          	.insn	4, 0x73736173

80004153 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.5>:
80004153:	7361                	.insn	2, 0x7361
80004155:	74726573          	.insn	4, 0x74726573
80004159:	6f69                	.insn	2, 0x6f69
8000415b:	206e                	.insn	2, 0x206e
8000415d:	6166                	.insn	2, 0x6166
8000415f:	6c69                	.insn	2, 0x6c69
80004161:	6465                	.insn	2, 0x6465
80004163:	203a                	.insn	2, 0x203a
80004165:	6465                	.insn	2, 0x6465
80004167:	682e6567          	.insn	4, 0x682e6567
8000416b:	6965                	.insn	2, 0x6965
8000416d:	20746867          	.insn	4, 0x20746867
80004171:	3d3d                	.insn	2, 0x3d3d
80004173:	7320                	.insn	2, 0x7320
80004175:	6c65                	.insn	2, 0x6c65
80004177:	2e66                	.insn	2, 0x2e66
80004179:	6568                	.insn	2, 0x6568
8000417b:	6769                	.insn	2, 0x6769
8000417d:	7468                	.insn	2, 0x7468
8000417f:	2d20                	.insn	2, 0x2d20
80004181:	3120                	.insn	2, 0x3120
	...

80004184 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.6>:
80004184:	40d4                	.insn	2, 0x40d4
80004186:	8000                	.insn	2, 0x8000
80004188:	007f 0000 02af 0000 	.insn	10, 0x0009000002af0000007f
80004190:	0009 
	...

80004194 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.7>:
80004194:	40d4                	.insn	2, 0x40d4
80004196:	8000                	.insn	2, 0x8000
80004198:	007f 0000 02b3 0000 	.insn	10, 0x0009000002b30000007f
800041a0:	0009 
	...

800041a4 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.8>:
800041a4:	7361                	.insn	2, 0x7361
800041a6:	74726573          	.insn	4, 0x74726573
800041aa:	6f69                	.insn	2, 0x6f69
800041ac:	206e                	.insn	2, 0x206e
800041ae:	6166                	.insn	2, 0x6166
800041b0:	6c69                	.insn	2, 0x6c69
800041b2:	6465                	.insn	2, 0x6465
800041b4:	203a                	.insn	2, 0x203a
800041b6:	2e637273          	.insn	4, 0x2e637273
800041ba:	656c                	.insn	2, 0x656c
800041bc:	286e                	.insn	2, 0x286e
800041be:	2029                	.insn	2, 0x2029
800041c0:	3d3d                	.insn	2, 0x3d3d
800041c2:	6420                	.insn	2, 0x6420
800041c4:	6c2e7473          	.insn	4, 0x6c2e7473
800041c8:	6e65                	.insn	2, 0x6e65
800041ca:	2928                	.insn	2, 0x2928

800041cc <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.9>:
800041cc:	40d4                	.insn	2, 0x40d4
800041ce:	8000                	.insn	2, 0x8000
800041d0:	007f 0000 073d 0000 	.insn	10, 0x00050000073d0000007f
800041d8:	0005 
	...

800041dc <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.10>:
800041dc:	40d4                	.insn	2, 0x40d4
800041de:	8000                	.insn	2, 0x8000
800041e0:	007f 0000 04bd 0000 	.insn	10, 0x0023000004bd0000007f
800041e8:	0023 
	...

800041ec <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.11>:
800041ec:	40d4                	.insn	2, 0x40d4
800041ee:	8000                	.insn	2, 0x8000
800041f0:	007f 0000 04fd 0000 	.insn	10, 0x0024000004fd0000007f
800041f8:	0024 
	...

800041fc <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.12>:
800041fc:	7361                	.insn	2, 0x7361
800041fe:	74726573          	.insn	4, 0x74726573
80004202:	6f69                	.insn	2, 0x6f69
80004204:	206e                	.insn	2, 0x206e
80004206:	6166                	.insn	2, 0x6166
80004208:	6c69                	.insn	2, 0x6c69
8000420a:	6465                	.insn	2, 0x6465
8000420c:	203a                	.insn	2, 0x203a
8000420e:	6465                	.insn	2, 0x6465
80004210:	682e6567          	.insn	4, 0x682e6567
80004214:	6965                	.insn	2, 0x6965
80004216:	20746867          	.insn	4, 0x20746867
8000421a:	3d3d                	.insn	2, 0x3d3d
8000421c:	7320                	.insn	2, 0x7320
8000421e:	6c65                	.insn	2, 0x6c65
80004220:	2e66                	.insn	2, 0x2e66
80004222:	6f6e                	.insn	2, 0x6f6e
80004224:	6564                	.insn	2, 0x6564
80004226:	682e                	.insn	2, 0x682e
80004228:	6965                	.insn	2, 0x6965
8000422a:	20746867          	.insn	4, 0x20746867
8000422e:	202d                	.insn	2, 0x202d
80004230:	0031                	.insn	2, 0x0031
	...

80004234 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.13>:
80004234:	40d4                	.insn	2, 0x40d4
80004236:	8000                	.insn	2, 0x8000
80004238:	007f 0000 03f0 0000 	.insn	10, 0x0009000003f00000007f
80004240:	0009 
	...

80004244 <anon.0967ae7c4fd660b9acdb752d1aeda62f.14.llvm.1023813754811204676>:
80004244:	6d6f682f          	.insn	4, 0x6d6f682f
80004248:	2f65                	.insn	2, 0x2f65
8000424a:	6572                	.insn	2, 0x6572
8000424c:	796d                	.insn	2, 0x796d
8000424e:	75722e2f          	.insn	4, 0x75722e2f
80004252:	70757473          	.insn	4, 0x70757473
80004256:	6f6f742f          	.insn	4, 0x6f6f742f
8000425a:	636c                	.insn	2, 0x636c
8000425c:	6168                	.insn	2, 0x6168
8000425e:	6e69                	.insn	2, 0x6e69
80004260:	696e2f73          	.insn	4, 0x696e2f73
80004264:	6c746867          	.insn	4, 0x6c746867
80004268:	2d79                	.insn	2, 0x2d79
8000426a:	3878                	.insn	2, 0x3878
8000426c:	5f36                	.insn	2, 0x5f36
8000426e:	3436                	.insn	2, 0x3436
80004270:	752d                	.insn	2, 0x752d
80004272:	6b6e                	.insn	2, 0x6b6e
80004274:	6f6e                	.insn	2, 0x6f6e
80004276:	6c2d6e77          	.insn	4, 0x6c2d6e77
8000427a:	6e69                	.insn	2, 0x6e69
8000427c:	7875                	.insn	2, 0x7875
8000427e:	672d                	.insn	2, 0x672d
80004280:	756e                	.insn	2, 0x756e
80004282:	62696c2f          	.insn	4, 0x62696c2f
80004286:	7375722f          	.insn	4, 0x7375722f
8000428a:	6c74                	.insn	2, 0x6c74
8000428c:	6269                	.insn	2, 0x6269
8000428e:	6372732f          	.insn	4, 0x6372732f
80004292:	7375722f          	.insn	4, 0x7375722f
80004296:	2f74                	.insn	2, 0x2f74
80004298:	696c                	.insn	2, 0x696c
8000429a:	7262                	.insn	2, 0x7262
8000429c:	7261                	.insn	2, 0x7261
8000429e:	2f79                	.insn	2, 0x2f79
800042a0:	6c61                	.insn	2, 0x6c61
800042a2:	6f6c                	.insn	2, 0x6f6c
800042a4:	72732f63          	.insn	4, 0x72732f63
800042a8:	6f632f63          	.insn	4, 0x6f632f63
800042ac:	6c6c                	.insn	2, 0x6c6c
800042ae:	6365                	.insn	2, 0x6365
800042b0:	6974                	.insn	2, 0x6974
800042b2:	2f736e6f          	jal	t3,8003ada8 <KALLOC_BUFFER+0x34da8>
800042b6:	7462                	.insn	2, 0x7462
800042b8:	6572                	.insn	2, 0x6572
800042ba:	2f65                	.insn	2, 0x2f65
800042bc:	616e                	.insn	2, 0x616e
800042be:	6976                	.insn	2, 0x6976
800042c0:	65746167          	.insn	4, 0x65746167
800042c4:	722e                	.insn	2, 0x722e
800042c6:	          	.insn	4, 0x42440073

800042c8 <anon.0967ae7c4fd660b9acdb752d1aeda62f.15.llvm.1023813754811204676>:
800042c8:	4244                	.insn	2, 0x4244
800042ca:	8000                	.insn	2, 0x8000
800042cc:	00000083          	lb	ra,0(zero) # 0 <.Lline_table_start0>
800042d0:	0258                	.insn	2, 0x0258
800042d2:	0000                	.insn	2, 0x
800042d4:	0030                	.insn	2, 0x0030
	...

800042d8 <.Lanon.272ad12a150edddff8aff02f5f98f349.2>:
800042d8:	614c                	.insn	2, 0x614c
800042da:	6f79                	.insn	2, 0x6f79
800042dc:	7475                	.insn	2, 0x7475
800042de:	7245                	.insn	2, 0x7245
800042e0:	6f72                	.insn	2, 0x6f72
800042e2:	                	.insn	2, 0x7372

800042e3 <anon.272ad12a150edddff8aff02f5f98f349.3.llvm.10321614113892589689>:
800042e3:	2f637273          	.insn	4, 0x2f637273
800042e7:	6f68                	.insn	2, 0x6f68
800042e9:	656c                	.insn	2, 0x656c
800042eb:	722e                	.insn	2, 0x722e
800042ed:	          	.insn	4, 0x696c6173

800042ee <.Lanon.272ad12a150edddff8aff02f5f98f349.4>:
800042ee:	6c61                	.insn	2, 0x6c61
800042f0:	6769                	.insn	2, 0x6769
800042f2:	5f6e                	.insn	2, 0x5f6e
800042f4:	7366666f          	jal	a2,8006aa2a <KALLOC_BUFFER+0x64a2a>
800042f8:	7465                	.insn	2, 0x7465
800042fa:	203a                	.insn	2, 0x203a
800042fc:	6c61                	.insn	2, 0x6c61
800042fe:	6769                	.insn	2, 0x6769
80004300:	206e                	.insn	2, 0x206e
80004302:	7369                	.insn	2, 0x7369
80004304:	6e20                	.insn	2, 0x6e20
80004306:	6120746f          	jal	s0,8000b918 <KALLOC_BUFFER+0x5918>
8000430a:	7020                	.insn	2, 0x7020
8000430c:	7265776f          	jal	a4,8005ba32 <KALLOC_BUFFER+0x55a32>
80004310:	6f2d                	.insn	2, 0x6f2d
80004312:	2d66                	.insn	2, 0x2d66
80004314:	7774                	.insn	2, 0x7774
80004316:	          	j	800e4744 <KALLOC_BUFFER+0xde744>

80004318 <.Lanon.272ad12a150edddff8aff02f5f98f349.5>:
80004318:	42ee                	.insn	2, 0x42ee
8000431a:	8000                	.insn	2, 0x8000
8000431c:	0029                	.insn	2, 0x0029
	...

80004320 <.Lanon.272ad12a150edddff8aff02f5f98f349.6>:
80004320:	6d6f682f          	.insn	4, 0x6d6f682f
80004324:	2f65                	.insn	2, 0x2f65
80004326:	6572                	.insn	2, 0x6572
80004328:	796d                	.insn	2, 0x796d
8000432a:	75722e2f          	.insn	4, 0x75722e2f
8000432e:	70757473          	.insn	4, 0x70757473
80004332:	6f6f742f          	.insn	4, 0x6f6f742f
80004336:	636c                	.insn	2, 0x636c
80004338:	6168                	.insn	2, 0x6168
8000433a:	6e69                	.insn	2, 0x6e69
8000433c:	696e2f73          	.insn	4, 0x696e2f73
80004340:	6c746867          	.insn	4, 0x6c746867
80004344:	2d79                	.insn	2, 0x2d79
80004346:	3878                	.insn	2, 0x3878
80004348:	5f36                	.insn	2, 0x5f36
8000434a:	3436                	.insn	2, 0x3436
8000434c:	752d                	.insn	2, 0x752d
8000434e:	6b6e                	.insn	2, 0x6b6e
80004350:	6f6e                	.insn	2, 0x6f6e
80004352:	6c2d6e77          	.insn	4, 0x6c2d6e77
80004356:	6e69                	.insn	2, 0x6e69
80004358:	7875                	.insn	2, 0x7875
8000435a:	672d                	.insn	2, 0x672d
8000435c:	756e                	.insn	2, 0x756e
8000435e:	62696c2f          	.insn	4, 0x62696c2f
80004362:	7375722f          	.insn	4, 0x7375722f
80004366:	6c74                	.insn	2, 0x6c74
80004368:	6269                	.insn	2, 0x6269
8000436a:	6372732f          	.insn	4, 0x6372732f
8000436e:	7375722f          	.insn	4, 0x7375722f
80004372:	2f74                	.insn	2, 0x2f74
80004374:	696c                	.insn	2, 0x696c
80004376:	7262                	.insn	2, 0x7262
80004378:	7261                	.insn	2, 0x7261
8000437a:	2f79                	.insn	2, 0x2f79
8000437c:	65726f63          	bltu	tp,s7,800049da <.Lanon.0a795d8d80343cc40e42ade3e02d1552.292+0xbf>
80004380:	6372732f          	.insn	4, 0x6372732f
80004384:	7274702f          	.insn	4, 0x7274702f
80004388:	74756d2f          	.insn	4, 0x74756d2f
8000438c:	705f 7274 722e      	.insn	6, 0x722e7274705f
80004392:	          	.insn	4, 0x43200073

80004394 <.Lanon.272ad12a150edddff8aff02f5f98f349.7>:
80004394:	4320                	.insn	2, 0x4320
80004396:	8000                	.insn	2, 0x8000
80004398:	00000073          	ecall
8000439c:	0666                	.insn	2, 0x0666
8000439e:	0000                	.insn	2, 0x
800043a0:	000d                	.insn	2, 0x000d
	...

800043a4 <.Lanon.272ad12a150edddff8aff02f5f98f349.12>:
	...
800043ac:	0001                	.insn	2, 0x0001
800043ae:	0000                	.insn	2, 0x
800043b0:	0978                	.insn	2, 0x0978
800043b2:	8000                	.insn	2, 0x8000

800043b4 <.Lanon.272ad12a150edddff8aff02f5f98f349.13>:
800043b4:	6c6c6163          	bltu	s8,t1,80004a76 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.469+0x6e>
800043b8:	6465                	.insn	2, 0x6465
800043ba:	6020                	.insn	2, 0x6020
800043bc:	6552                	.insn	2, 0x6552
800043be:	746c7573          	.insn	4, 0x746c7573
800043c2:	3a3a                	.insn	2, 0x3a3a
800043c4:	6e75                	.insn	2, 0x6e75
800043c6:	70617277          	.insn	4, 0x70617277
800043ca:	2928                	.insn	2, 0x2928
800043cc:	2060                	.insn	2, 0x2060
800043ce:	61206e6f          	jal	t3,8000a9e0 <KALLOC_BUFFER+0x49e0>
800043d2:	206e                	.insn	2, 0x206e
800043d4:	4560                	.insn	2, 0x4560
800043d6:	7272                	.insn	2, 0x7272
800043d8:	2060                	.insn	2, 0x2060
800043da:	6176                	.insn	2, 0x6176
800043dc:	756c                	.insn	2, 0x756c
800043de:	0065                	.insn	2, 0x0065

800043e0 <.Lanon.272ad12a150edddff8aff02f5f98f349.14>:
800043e0:	800042e3          	bltz	zero,80003be4 <memcpy+0x5c>
800043e4:	0000000b          	.insn	4, 0x000b
800043e8:	01b8                	.insn	2, 0x01b8
800043ea:	0000                	.insn	2, 0x
800043ec:	0039                	.insn	2, 0x0039
	...

800043f0 <.Lanon.272ad12a150edddff8aff02f5f98f349.15>:
800043f0:	7246                	.insn	2, 0x7246
800043f2:	6565                	.insn	2, 0x6565
800043f4:	2064                	.insn	2, 0x2064
800043f6:	6f6e                	.insn	2, 0x6f6e
800043f8:	6564                	.insn	2, 0x6564
800043fa:	6120                	.insn	2, 0x6120
800043fc:	696c                	.insn	2, 0x696c
800043fe:	7361                	.insn	2, 0x7361
80004400:	7365                	.insn	2, 0x7365
80004402:	6520                	.insn	2, 0x6520
80004404:	6978                	.insn	2, 0x6978
80004406:	6e697473          	.insn	4, 0x6e697473
8000440a:	6f682067          	.insn	4, 0x6f682067
8000440e:	656c                	.insn	2, 0x656c
80004410:	2021                	.insn	2, 0x2021
80004412:	6142                	.insn	2, 0x6142
80004414:	2064                	.insn	2, 0x2064
80004416:	7266                	.insn	2, 0x7266
80004418:	6565                	.insn	2, 0x6565
8000441a:	  	.insn	8, 0x002b800043f0003f

8000441c <.Lanon.272ad12a150edddff8aff02f5f98f349.16>:
8000441c:	43f0                	.insn	2, 0x43f0
8000441e:	8000                	.insn	2, 0x8000
80004420:	0000002b          	.insn	4, 0x002b

80004424 <.Lanon.272ad12a150edddff8aff02f5f98f349.17>:
80004424:	800042e3          	bltz	zero,80003c28 <memcpy+0xa0>
80004428:	0000000b          	.insn	4, 0x000b
8000442c:	0206                	.insn	2, 0x0206
8000442e:	0000                	.insn	2, 0x
80004430:	000d                	.insn	2, 0x000d
	...

80004434 <.Lanon.272ad12a150edddff8aff02f5f98f349.18>:
80004434:	800042e3          	bltz	zero,80003c38 <memcpy+0xb0>
80004438:	0000000b          	.insn	4, 0x000b
8000443c:	0228                	.insn	2, 0x0228
8000443e:	0000                	.insn	2, 0x
80004440:	0011                	.insn	2, 0x0011
	...

80004444 <.Lanon.272ad12a150edddff8aff02f5f98f349.19>:
80004444:	7246                	.insn	2, 0x7246
80004446:	6565                	.insn	2, 0x6565
80004448:	2064                	.insn	2, 0x2064
8000444a:	6f6e                	.insn	2, 0x6f6e
8000444c:	6564                	.insn	2, 0x6564
8000444e:	2820                	.insn	2, 0x2820

80004450 <.Lanon.272ad12a150edddff8aff02f5f98f349.20>:
80004450:	2029                	.insn	2, 0x2029
80004452:	6c61                	.insn	2, 0x6c61
80004454:	6169                	.insn	2, 0x6169
80004456:	20736573          	.insn	4, 0x20736573
8000445a:	7865                	.insn	2, 0x7865
8000445c:	7369                	.insn	2, 0x7369
8000445e:	6974                	.insn	2, 0x6974
80004460:	676e                	.insn	2, 0x676e
80004462:	6820                	.insn	2, 0x6820
80004464:	20656c6f          	jal	s8,8005a66a <KALLOC_BUFFER+0x5466a>
80004468:	                	.insn	2, 0x5b28

80004469 <.Lanon.272ad12a150edddff8aff02f5f98f349.21>:
80004469:	          	.insn	4, 0x21295d5b

8000446a <.Lanon.272ad12a150edddff8aff02f5f98f349.22>:
8000446a:	295d                	.insn	2, 0x295d
8000446c:	2021                	.insn	2, 0x2021
8000446e:	6142                	.insn	2, 0x6142
80004470:	2064                	.insn	2, 0x2064
80004472:	7266                	.insn	2, 0x7266
80004474:	6565                	.insn	2, 0x6565
80004476:	  	.insn	8, 0x000c80004444003f

80004478 <.Lanon.272ad12a150edddff8aff02f5f98f349.23>:
80004478:	4444                	.insn	2, 0x4444
8000447a:	8000                	.insn	2, 0x8000
8000447c:	000c                	.insn	2, 0x000c
8000447e:	0000                	.insn	2, 0x
80004480:	4450                	.insn	2, 0x4450
80004482:	8000                	.insn	2, 0x8000
80004484:	0019                	.insn	2, 0x0019
80004486:	0000                	.insn	2, 0x
80004488:	4469                	.insn	2, 0x4469
8000448a:	8000                	.insn	2, 0x8000
8000448c:	0001                	.insn	2, 0x0001
8000448e:	0000                	.insn	2, 0x
80004490:	446a                	.insn	2, 0x446a
80004492:	8000                	.insn	2, 0x8000
80004494:	000d                	.insn	2, 0x000d
	...

80004498 <.Lanon.272ad12a150edddff8aff02f5f98f349.24>:
80004498:	800042e3          	bltz	zero,80003c9c <memmove+0x4>
8000449c:	0000000b          	.insn	4, 0x000b
800044a0:	0000023b          	.insn	4, 0x023b
800044a4:	0009                	.insn	2, 0x0009
	...

800044a8 <.Lanon.272ad12a150edddff8aff02f5f98f349.25>:
800044a8:	6170                	.insn	2, 0x6170
800044aa:	6c6c                	.insn	2, 0x6c6c
800044ac:	203a636f          	jal	t1,800aaeae <KALLOC_BUFFER+0xa4eae>
800044b0:	6966                	.insn	2, 0x6966
800044b2:	7372                	.insn	2, 0x7372
800044b4:	2074                	.insn	2, 0x2074

800044b6 <.Lanon.272ad12a150edddff8aff02f5f98f349.26>:
800044b6:	6c20                	.insn	2, 0x6c20
800044b8:	7361                	.insn	2, 0x7361
800044ba:	2074                	.insn	2, 0x2074

800044bc <.Lanon.272ad12a150edddff8aff02f5f98f349.27>:
800044bc:	000a                	.insn	2, 0x000a
	...

800044c0 <.Lanon.272ad12a150edddff8aff02f5f98f349.28>:
800044c0:	44a8                	.insn	2, 0x44a8
800044c2:	8000                	.insn	2, 0x8000
800044c4:	000e                	.insn	2, 0x000e
800044c6:	0000                	.insn	2, 0x
800044c8:	44b6                	.insn	2, 0x44b6
800044ca:	8000                	.insn	2, 0x8000
800044cc:	0006                	.insn	2, 0x0006
800044ce:	0000                	.insn	2, 0x
800044d0:	44bc                	.insn	2, 0x44bc
800044d2:	8000                	.insn	2, 0x8000
800044d4:	0001                	.insn	2, 0x0001
	...

800044d8 <anon.272ad12a150edddff8aff02f5f98f349.29.llvm.10321614113892589689>:
800044d8:	2f637273          	.insn	4, 0x2f637273
800044dc:	6f70                	.insn	2, 0x6f70
800044de:	6e69                	.insn	2, 0x6e69
800044e0:	6574                	.insn	2, 0x6574
800044e2:	2e72                	.insn	2, 0x2e72
800044e4:	7372                	.insn	2, 0x7372
	...

800044e8 <anon.272ad12a150edddff8aff02f5f98f349.30.llvm.10321614113892589689>:
800044e8:	44d8                	.insn	2, 0x44d8
800044ea:	8000                	.insn	2, 0x8000
800044ec:	000e                	.insn	2, 0x000e
800044ee:	0000                	.insn	2, 0x
800044f0:	0000002f          	.insn	4, 0x002f
800044f4:	0030                	.insn	2, 0x0030
	...

800044f8 <.Lanon.272ad12a150edddff8aff02f5f98f349.31>:
800044f8:	6170                	.insn	2, 0x6170
800044fa:	6c6c                	.insn	2, 0x6c6c
800044fc:	6220636f          	jal	t1,8000ab1e <KALLOC_BUFFER+0x4b1e>
80004500:	7361                	.insn	2, 0x7361
80004502:	3a65                	.insn	2, 0x3a65
80004504:	3020                	.insn	2, 0x3020
80004506:	0078                	.insn	2, 0x0078

80004508 <.Lanon.272ad12a150edddff8aff02f5f98f349.32>:
80004508:	44f8                	.insn	2, 0x44f8
8000450a:	8000                	.insn	2, 0x8000
8000450c:	0000000f          	fence	unknown,unknown
80004510:	44bc                	.insn	2, 0x44bc
80004512:	8000                	.insn	2, 0x8000
80004514:	0001                	.insn	2, 0x0001
	...

80004518 <.Lanon.58335f229ba488831bc287488e11d397.4>:
80004518:	7245                	.insn	2, 0x7245
8000451a:	6f72                	.insn	2, 0x6f72
8000451c:	0072                	.insn	2, 0x0072
	...

80004520 <.Lanon.58335f229ba488831bc287488e11d397.5>:
80004520:	0000                	.insn	2, 0x
80004522:	0000                	.insn	2, 0x
80004524:	0004                	.insn	2, 0x0004
80004526:	0000                	.insn	2, 0x
80004528:	0004                	.insn	2, 0x0004
8000452a:	0000                	.insn	2, 0x
8000452c:	13a4                	.insn	2, 0x13a4
8000452e:	8000                	.insn	2, 0x8000

80004530 <.Lanon.58335f229ba488831bc287488e11d397.6>:
80004530:	0000                	.insn	2, 0x
80004532:	0000                	.insn	2, 0x
80004534:	0004                	.insn	2, 0x0004
80004536:	0000                	.insn	2, 0x
80004538:	0004                	.insn	2, 0x0004
8000453a:	0000                	.insn	2, 0x
8000453c:	080c                	.insn	2, 0x080c
8000453e:	8000                	.insn	2, 0x8000

80004540 <.Lanon.58335f229ba488831bc287488e11d397.7>:
80004540:	614c                	.insn	2, 0x614c
80004542:	6f79                	.insn	2, 0x6f79
80004544:	7475                	.insn	2, 0x7475
80004546:	2020                	.insn	2, 0x2020
80004548:	2020                	.insn	2, 0x2020
8000454a:	657a6973          	.insn	4, 0x657a6973

8000454e <.Lanon.58335f229ba488831bc287488e11d397.9>:
8000454e:	6c61                	.insn	2, 0x6c61
80004550:	6769                	.insn	2, 0x6769
80004552:	                	.insn	2, 0x496e

80004553 <.Lanon.58335f229ba488831bc287488e11d397.10>:
80004553:	6e49                	.insn	2, 0x6e49
80004555:	75727473          	.insn	4, 0x75727473
80004559:	6f697463          	bgeu	s2,s6,80004c41 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.469+0x239>
8000455d:	4d6e                	.insn	2, 0x4d6e
8000455f:	7369                	.insn	2, 0x7369
80004561:	6c61                	.insn	2, 0x6c61
80004563:	6769                	.insn	2, 0x6769
80004565:	656e                	.insn	2, 0x656e
80004567:	4964                	.insn	2, 0x4964
80004569:	736e                	.insn	2, 0x736e
8000456b:	7274                	.insn	2, 0x7274
8000456d:	6375                	.insn	2, 0x6375
8000456f:	6974                	.insn	2, 0x6974
80004571:	61466e6f          	jal	t3,8006ab85 <KALLOC_BUFFER+0x64b85>
80004575:	6c75                	.insn	2, 0x6c75
80004577:	7274                	.insn	2, 0x7274
80004579:	6e61                	.insn	2, 0x6e61
8000457b:	65206567          	.insn	4, 0x65206567
8000457f:	646e                	.insn	2, 0x646e
80004581:	6920                	.insn	2, 0x6920
80004583:	646e                	.insn	2, 0x646e
80004585:	7865                	.insn	2, 0x7865
80004587:	0020                	.insn	2, 0x0020
80004589:	0201                	.insn	2, 0x0201
8000458b:	06050403          	lb	s0,96(a0)
8000458f:	0e090807          	.insn	4, 0x0e090807
80004593:	0b0a                	.insn	2, 0x0b0a
80004595:	0e0c                	.insn	2, 0x0e0c
80004597:	                	.insn	2, 0x490d

80004598 <.Lanon.58335f229ba488831bc287488e11d397.12>:
80004598:	6c49                	.insn	2, 0x6c49
8000459a:	656c                	.insn	2, 0x656c
8000459c:	496c6167          	.insn	4, 0x496c6167
800045a0:	736e                	.insn	2, 0x736e
800045a2:	7274                	.insn	2, 0x7274
800045a4:	6375                	.insn	2, 0x6375
800045a6:	6974                	.insn	2, 0x6974
800045a8:	          	jal	t3,8002accc <KALLOC_BUFFER+0x24ccc>

800045aa <.Lanon.58335f229ba488831bc287488e11d397.13>:
800045aa:	7242                	.insn	2, 0x7242
800045ac:	6165                	.insn	2, 0x6165
800045ae:	696f706b          	.insn	4, 0x696f706b
800045b2:	746e                	.insn	2, 0x746e

800045b4 <.Lanon.58335f229ba488831bc287488e11d397.14>:
800045b4:	6f4c                	.insn	2, 0x6f4c
800045b6:	6461                	.insn	2, 0x6461
800045b8:	694d                	.insn	2, 0x694d
800045ba:	696c6173          	.insn	4, 0x696c6173
800045be:	64656e67          	.insn	4, 0x64656e67

800045c2 <.Lanon.58335f229ba488831bc287488e11d397.15>:
800045c2:	6f4c                	.insn	2, 0x6f4c
800045c4:	6461                	.insn	2, 0x6461
800045c6:	6146                	.insn	2, 0x6146
800045c8:	6c75                	.insn	2, 0x6c75
800045ca:	                	.insn	2, 0x5374

800045cb <.Lanon.58335f229ba488831bc287488e11d397.16>:
800045cb:	726f7453          	.insn	4, 0x726f7453
800045cf:	4d65                	.insn	2, 0x4d65
800045d1:	7369                	.insn	2, 0x7369
800045d3:	6c61                	.insn	2, 0x6c61
800045d5:	6769                	.insn	2, 0x6769
800045d7:	656e                	.insn	2, 0x656e
800045d9:	                	.insn	2, 0x5364

800045da <.Lanon.58335f229ba488831bc287488e11d397.17>:
800045da:	726f7453          	.insn	4, 0x726f7453
800045de:	4665                	.insn	2, 0x4665
800045e0:	7561                	.insn	2, 0x7561
800045e2:	746c                	.insn	2, 0x746c

800045e4 <.Lanon.58335f229ba488831bc287488e11d397.18>:
800045e4:	7355                	.insn	2, 0x7355
800045e6:	7265                	.insn	2, 0x7265
800045e8:	6e45                	.insn	2, 0x6e45
800045ea:	4376                	.insn	2, 0x4376
800045ec:	6c61                	.insn	2, 0x6c61
800045ee:	                	.insn	2, 0x536c

800045ef <.Lanon.58335f229ba488831bc287488e11d397.19>:
800045ef:	65707553          	.insn	4, 0x65707553
800045f3:	7672                	.insn	2, 0x7672
800045f5:	7369                	.insn	2, 0x7369
800045f7:	6e45726f          	jal	tp,8005bcdb <KALLOC_BUFFER+0x55cdb>
800045fb:	4376                	.insn	2, 0x4376
800045fd:	6c61                	.insn	2, 0x6c61
800045ff:	                	.insn	2, 0x4d6c

80004600 <.Lanon.58335f229ba488831bc287488e11d397.20>:
80004600:	614d                	.insn	2, 0x614d
80004602:	6e696863          	bltu	s2,t1,80004cf2 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.469+0x2ea>
80004606:	4565                	.insn	2, 0x4565
80004608:	766e                	.insn	2, 0x766e
8000460a:	6c6c6143          	.insn	4, 0x6c6c6143

8000460e <.Lanon.58335f229ba488831bc287488e11d397.21>:
8000460e:	6e49                	.insn	2, 0x6e49
80004610:	75727473          	.insn	4, 0x75727473
80004614:	6f697463          	bgeu	s2,s6,80004cfc <.Lanon.0a795d8d80343cc40e42ade3e02d1552.469+0x2f4>
80004618:	506e                	.insn	2, 0x506e
8000461a:	6761                	.insn	2, 0x6761
8000461c:	4665                	.insn	2, 0x4665
8000461e:	7561                	.insn	2, 0x7561
80004620:	746c                	.insn	2, 0x746c

80004622 <.Lanon.58335f229ba488831bc287488e11d397.22>:
80004622:	6f4c                	.insn	2, 0x6f4c
80004624:	6461                	.insn	2, 0x6461
80004626:	6150                	.insn	2, 0x6150
80004628:	61466567          	.insn	4, 0x61466567
8000462c:	6c75                	.insn	2, 0x6c75
8000462e:	                	.insn	2, 0x5374

8000462f <.Lanon.58335f229ba488831bc287488e11d397.23>:
8000462f:	726f7453          	.insn	4, 0x726f7453
80004633:	5065                	.insn	2, 0x5065
80004635:	6761                	.insn	2, 0x6761
80004637:	4665                	.insn	2, 0x4665
80004639:	7561                	.insn	2, 0x7561
8000463b:	746c                	.insn	2, 0x746c

8000463d <.Lanon.58335f229ba488831bc287488e11d397.24>:
8000463d:	6e55                	.insn	2, 0x6e55
8000463f:	776f6e6b          	.insn	4, 0x776f6e6b
80004643:	                	.insn	2, 0x006e

80004644 <anon.58335f229ba488831bc287488e11d397.25.llvm.14510610307159664504>:
	...
8000464c:	0001                	.insn	2, 0x0001
8000464e:	0000                	.insn	2, 0x
80004650:	1590                	.insn	2, 0x1590
80004652:	8000                	.insn	2, 0x8000
80004654:	13d0                	.insn	2, 0x13d0
80004656:	8000                	.insn	2, 0x8000
80004658:	14b0                	.insn	2, 0x14b0
8000465a:	8000                	.insn	2, 0x8000

8000465c <anon.58335f229ba488831bc287488e11d397.26.llvm.14510610307159664504>:
	...
80004664:	0001                	.insn	2, 0x0001
80004666:	0000                	.insn	2, 0x
80004668:	14cc                	.insn	2, 0x14cc
8000466a:	8000                	.insn	2, 0x8000

8000466c <anon.58335f229ba488831bc287488e11d397.27.llvm.14510610307159664504>:
8000466c:	6c6c6163          	bltu	s8,t1,80004d2e <.Lanon.0a795d8d80343cc40e42ade3e02d1552.469+0x326>
80004670:	6465                	.insn	2, 0x6465
80004672:	6020                	.insn	2, 0x6020
80004674:	6552                	.insn	2, 0x6552
80004676:	746c7573          	.insn	4, 0x746c7573
8000467a:	3a3a                	.insn	2, 0x3a3a
8000467c:	6e75                	.insn	2, 0x6e75
8000467e:	70617277          	.insn	4, 0x70617277
80004682:	2928                	.insn	2, 0x2928
80004684:	2060                	.insn	2, 0x2060
80004686:	61206e6f          	jal	t3,8000ac98 <KALLOC_BUFFER+0x4c98>
8000468a:	206e                	.insn	2, 0x206e
8000468c:	4560                	.insn	2, 0x4560
8000468e:	7272                	.insn	2, 0x7272
80004690:	2060                	.insn	2, 0x2060
80004692:	6176                	.insn	2, 0x6176
80004694:	756c                	.insn	2, 0x756c
80004696:	                	.insn	2, 0x7365

80004697 <anon.58335f229ba488831bc287488e11d397.28.llvm.14510610307159664504>:
80004697:	2f637273          	.insn	4, 0x2f637273
8000469b:	7270                	.insn	2, 0x7270
8000469d:	6e69                	.insn	2, 0x6e69
8000469f:	6574                	.insn	2, 0x6574
800046a1:	2e72                	.insn	2, 0x2e72
800046a3:	7372                	.insn	2, 0x7372
800046a5:	0000                	.insn	2, 0x
	...

800046a8 <anon.58335f229ba488831bc287488e11d397.29.llvm.14510610307159664504>:
800046a8:	80004697          	auipc	a3,0x80004
800046ac:	000e                	.insn	2, 0x000e
800046ae:	0000                	.insn	2, 0x
800046b0:	00000017          	auipc	zero,0x0
800046b4:	001c                	.insn	2, 0x001c
	...

800046b8 <.Lanon.58335f229ba488831bc287488e11d397.30>:
800046b8:	636d                	.insn	2, 0x636d
800046ba:	7561                	.insn	2, 0x7561
800046bc:	203a6573          	.insn	4, 0x203a6573
800046c0:	7865                	.insn	2, 0x7865
800046c2:	74706563          	bltu	zero,t2,80004e0c <.Lanon.0a795d8d80343cc40e42ade3e02d1552.469+0x404>
800046c6:	6f69                	.insn	2, 0x6f69
800046c8:	206e                	.insn	2, 0x206e
800046ca:	7461                	.insn	2, 0x7461
800046cc:	                	.insn	2, 0x2020

800046cd <.Lanon.58335f229ba488831bc287488e11d397.31>:
800046cd:	                	.insn	2, 0x0a20

800046ce <.Lanon.58335f229ba488831bc287488e11d397.32>:
800046ce:	000a                	.insn	2, 0x000a

800046d0 <.Lanon.58335f229ba488831bc287488e11d397.33>:
800046d0:	46b8                	.insn	2, 0x46b8
800046d2:	8000                	.insn	2, 0x8000
800046d4:	0015                	.insn	2, 0x0015
800046d6:	0000                	.insn	2, 0x
800046d8:	46cd                	.insn	2, 0x46cd
800046da:	8000                	.insn	2, 0x8000
800046dc:	0001                	.insn	2, 0x0001
800046de:	0000                	.insn	2, 0x
800046e0:	46ce                	.insn	2, 0x46ce
800046e2:	8000                	.insn	2, 0x8000
800046e4:	0001                	.insn	2, 0x0001
	...

800046e8 <.Lanon.58335f229ba488831bc287488e11d397.34>:
800046e8:	6c6c616b          	.insn	4, 0x6c6c616b
800046ec:	6220636f          	jal	t1,8000ad0e <KALLOC_BUFFER+0x4d0e>
800046f0:	6675                	.insn	2, 0x6675
800046f2:	6566                	.insn	2, 0x6566
800046f4:	2072                	.insn	2, 0x2072
800046f6:	6162                	.insn	2, 0x6162
800046f8:	203a6573          	.insn	4, 0x203a6573

800046fc <.Lanon.58335f229ba488831bc287488e11d397.35>:
800046fc:	46e8                	.insn	2, 0x46e8
800046fe:	8000                	.insn	2, 0x8000
80004700:	0014                	.insn	2, 0x0014
80004702:	0000                	.insn	2, 0x
80004704:	46ce                	.insn	2, 0x46ce
80004706:	8000                	.insn	2, 0x8000
80004708:	0001                	.insn	2, 0x0001
	...

8000470c <.Lanon.58335f229ba488831bc287488e11d397.36>:
8000470c:	6f66                	.insn	2, 0x6f66
8000470e:	6e75                	.insn	2, 0x6e75
80004710:	2064                	.insn	2, 0x2064

80004712 <.Lanon.58335f229ba488831bc287488e11d397.37>:
80004712:	6520                	.insn	2, 0x6520
80004714:	656c                	.insn	2, 0x656c
80004716:	656d                	.insn	2, 0x656d
80004718:	746e                	.insn	2, 0x746e
8000471a:	6e692073          	.insn	4, 0x6e692073
8000471e:	7420                	.insn	2, 0x7420
80004720:	6568                	.insn	2, 0x6568
80004722:	6c20                	.insn	2, 0x6c20
80004724:	7369                	.insn	2, 0x7369
80004726:	0a74                	.insn	2, 0x0a74

80004728 <.Lanon.58335f229ba488831bc287488e11d397.38>:
80004728:	470c                	.insn	2, 0x470c
8000472a:	8000                	.insn	2, 0x8000
8000472c:	0006                	.insn	2, 0x0006
8000472e:	0000                	.insn	2, 0x
80004730:	4712                	.insn	2, 0x4712
80004732:	8000                	.insn	2, 0x8000
80004734:	0016                	.insn	2, 0x0016
	...

80004738 <.Lanon.58335f229ba488831bc287488e11d397.39>:
80004738:	31335b1b          	.insn	4, 0x31335b1b
8000473c:	4b6d                	.insn	2, 0x4b6d
8000473e:	5245                	.insn	2, 0x5245
80004740:	454e                	.insn	2, 0x454e
80004742:	204c                	.insn	2, 0x204c
80004744:	4150                	.insn	2, 0x4150
80004746:	494e                	.insn	2, 0x494e
80004748:	5b1b3a43          	.insn	4, 0x5b1b3a43
8000474c:	6d30                	.insn	2, 0x6d30
8000474e:	0020                	.insn	2, 0x0020

80004750 <.Lanon.58335f229ba488831bc287488e11d397.40>:
80004750:	4738                	.insn	2, 0x4738
80004752:	8000                	.insn	2, 0x8000
80004754:	00000017          	auipc	zero,0x0
80004758:	46ce                	.insn	2, 0x46ce
8000475a:	8000                	.insn	2, 0x8000
8000475c:	0001                	.insn	2, 0x0001
	...

80004760 <.Lanon.58335f229ba488831bc287488e11d397.43>:
80004760:	2f637273          	.insn	4, 0x2f637273
80004764:	616d                	.insn	2, 0x616d
80004766:	6e69                	.insn	2, 0x6e69
80004768:	722e                	.insn	2, 0x722e
8000476a:	          	.insn	4, 0x47600073

8000476c <.Lanon.58335f229ba488831bc287488e11d397.44>:
8000476c:	4760                	.insn	2, 0x4760
8000476e:	8000                	.insn	2, 0x8000
80004770:	0000000b          	.insn	4, 0x000b
80004774:	0000009b          	.insn	4, 0x009b
80004778:	0026                	.insn	2, 0x0026
	...

8000477c <.Lanon.58335f229ba488831bc287488e11d397.45>:
8000477c:	7375                	.insn	2, 0x7375
8000477e:	7265                	.insn	2, 0x7265
80004780:	6d5f 6961 3a6e      	.insn	6, 0x3a6e69616d5f
80004786:	0020                	.insn	2, 0x0020

80004788 <.Lanon.58335f229ba488831bc287488e11d397.46>:
80004788:	477c                	.insn	2, 0x477c
8000478a:	8000                	.insn	2, 0x8000
8000478c:	0000000b          	.insn	4, 0x000b
80004790:	46ce                	.insn	2, 0x46ce
80004792:	8000                	.insn	2, 0x8000
80004794:	0001                	.insn	2, 0x0001
	...

80004798 <.Lanon.58335f229ba488831bc287488e11d397.47>:
80004798:	6974                	.insn	2, 0x6974
8000479a:	656d                	.insn	2, 0x656d
8000479c:	203a                	.insn	2, 0x203a

8000479e <.Lanon.58335f229ba488831bc287488e11d397.48>:
8000479e:	6920                	.insn	2, 0x6920
800047a0:	736e                	.insn	2, 0x736e
800047a2:	7274                	.insn	2, 0x7274
800047a4:	7465                	.insn	2, 0x7465
800047a6:	203a                	.insn	2, 0x203a

800047a8 <.Lanon.58335f229ba488831bc287488e11d397.49>:
800047a8:	4798                	.insn	2, 0x4798
800047aa:	8000                	.insn	2, 0x8000
800047ac:	0006                	.insn	2, 0x0006
800047ae:	0000                	.insn	2, 0x
800047b0:	479e                	.insn	2, 0x479e
800047b2:	8000                	.insn	2, 0x8000
800047b4:	000a                	.insn	2, 0x000a
800047b6:	0000                	.insn	2, 0x
800047b8:	46ce                	.insn	2, 0x46ce
800047ba:	8000                	.insn	2, 0x8000
800047bc:	0001                	.insn	2, 0x0001
	...

800047c0 <.Lanon.58335f229ba488831bc287488e11d397.50>:
800047c0:	6c6c616b          	.insn	4, 0x6c6c616b
800047c4:	6620636f          	jal	t1,8000ae26 <KALLOC_BUFFER+0x4e26>
800047c8:	6961                	.insn	2, 0x6961
800047ca:	3a6c                	.insn	2, 0x3a6c
800047cc:	0020                	.insn	2, 0x0020
	...

800047d0 <.Lanon.58335f229ba488831bc287488e11d397.51>:
800047d0:	47c0                	.insn	2, 0x47c0
800047d2:	8000                	.insn	2, 0x8000
800047d4:	000d                	.insn	2, 0x000d
	...

800047d8 <.Lanon.58335f229ba488831bc287488e11d397.52>:
800047d8:	2f637273          	.insn	4, 0x2f637273
800047dc:	6c6c616b          	.insn	4, 0x6c6c616b
800047e0:	722e636f          	jal	t1,800eaf02 <KALLOC_BUFFER+0xe4f02>
800047e4:	00000073          	ecall

800047e8 <.Lanon.58335f229ba488831bc287488e11d397.53>:
800047e8:	47d8                	.insn	2, 0x47d8
800047ea:	8000                	.insn	2, 0x8000
800047ec:	000d                	.insn	2, 0x000d
800047ee:	0000                	.insn	2, 0x
800047f0:	0019                	.insn	2, 0x0019
800047f2:	0000                	.insn	2, 0x
800047f4:	0005                	.insn	2, 0x0005
	...

800047f8 <.Lswitch.table._ZN71_$LT$riscv..register..mcause..Exception$u20$as$u20$core..fmt..Debug$GT$3fmt17h43f79eca3d356742E>:
800047f8:	0015                	.insn	2, 0x0015
800047fa:	0000                	.insn	2, 0x
800047fc:	0010                	.insn	2, 0x0010
800047fe:	0000                	.insn	2, 0x
80004800:	0012                	.insn	2, 0x0012
80004802:	0000                	.insn	2, 0x
80004804:	000a                	.insn	2, 0x000a
80004806:	0000                	.insn	2, 0x
80004808:	000e                	.insn	2, 0x000e
8000480a:	0000                	.insn	2, 0x
8000480c:	0009                	.insn	2, 0x0009
8000480e:	0000                	.insn	2, 0x
80004810:	0000000f          	fence	unknown,unknown
80004814:	000a                	.insn	2, 0x000a
80004816:	0000                	.insn	2, 0x
80004818:	0000000b          	.insn	4, 0x000b
8000481c:	0011                	.insn	2, 0x0011
8000481e:	0000                	.insn	2, 0x
80004820:	000e                	.insn	2, 0x000e
80004822:	0000                	.insn	2, 0x
80004824:	0014                	.insn	2, 0x0014
80004826:	0000                	.insn	2, 0x
80004828:	000d                	.insn	2, 0x000d
8000482a:	0000                	.insn	2, 0x
8000482c:	000e                	.insn	2, 0x000e
8000482e:	0000                	.insn	2, 0x
80004830:	00000007          	.insn	4, 0x0007

80004834 <.Lswitch.table._ZN71_$LT$riscv..register..mcause..Exception$u20$as$u20$core..fmt..Debug$GT$3fmt17h43f79eca3d356742E.15>:
80004834:	80004553          	.insn	4, 0x80004553
80004838:	4568                	.insn	2, 0x4568
8000483a:	8000                	.insn	2, 0x8000
8000483c:	4598                	.insn	2, 0x4598
8000483e:	8000                	.insn	2, 0x8000
80004840:	45aa                	.insn	2, 0x45aa
80004842:	8000                	.insn	2, 0x8000
80004844:	45b4                	.insn	2, 0x45b4
80004846:	8000                	.insn	2, 0x8000
80004848:	45c2                	.insn	2, 0x45c2
8000484a:	8000                	.insn	2, 0x8000
8000484c:	800045cb          	.insn	4, 0x800045cb
80004850:	45da                	.insn	2, 0x45da
80004852:	8000                	.insn	2, 0x8000
80004854:	45e4                	.insn	2, 0x45e4
80004856:	8000                	.insn	2, 0x8000
80004858:	800045ef          	jal	a1,7ff08858 <.Lline_table_start2+0x7ff074b9>
8000485c:	4600                	.insn	2, 0x4600
8000485e:	8000                	.insn	2, 0x8000
80004860:	460e                	.insn	2, 0x460e
80004862:	8000                	.insn	2, 0x8000
80004864:	4622                	.insn	2, 0x4622
80004866:	8000                	.insn	2, 0x8000
80004868:	8000462f          	.insn	4, 0x8000462f
8000486c:	463d                	.insn	2, 0x463d
8000486e:	8000                	.insn	2, 0x8000

80004870 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.138>:
80004870:	2820                	.insn	2, 0x2820
80004872:	2031                	.insn	2, 0x2031
80004874:	3c3c                	.insn	2, 0x3c3c
80004876:	                	.insn	2, 0x2920

80004877 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.139>:
80004877:	                	.insn	2, 0x0129

80004878 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.140>:
80004878:	0001                	.insn	2, 0x0001
8000487a:	0000                	.insn	2, 0x
8000487c:	0000                	.insn	2, 0x
8000487e:	0000                	.insn	2, 0x
80004880:	4870                	.insn	2, 0x4870
80004882:	8000                	.insn	2, 0x8000
80004884:	00000007          	.insn	4, 0x0007
80004888:	80004877          	.insn	4, 0x80004877
8000488c:	0001                	.insn	2, 0x0001
	...

80004890 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.210>:
80004890:	                	.insn	2, 0x633a

80004891 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.220>:
80004891:	6c6c6163          	bltu	s8,t1,80004f53 <__rust_no_alloc_shim_is_unstable+0x25>
80004895:	6465                	.insn	2, 0x6465
80004897:	6020                	.insn	2, 0x6020
80004899:	6974704f          	.insn	4, 0x6974704f
8000489d:	3a3a6e6f          	jal	t3,800ab43f <KALLOC_BUFFER+0xa543f>
800048a1:	6e75                	.insn	2, 0x6e75
800048a3:	70617277          	.insn	4, 0x70617277
800048a7:	2928                	.insn	2, 0x2928
800048a9:	2060                	.insn	2, 0x2060
800048ab:	61206e6f          	jal	t3,8000aebd <KALLOC_BUFFER+0x4ebd>
800048af:	6020                	.insn	2, 0x6020
800048b1:	6f4e                	.insn	2, 0x6f4e
800048b3:	656e                	.insn	2, 0x656e
800048b5:	2060                	.insn	2, 0x2060
800048b7:	6176                	.insn	2, 0x6176
800048b9:	756c                	.insn	2, 0x756c
800048bb:	                	.insn	2, 0x0165

800048bc <.Lanon.0a795d8d80343cc40e42ade3e02d1552.221>:
800048bc:	0001                	.insn	2, 0x0001
800048be:	0000                	.insn	2, 0x
800048c0:	0000                	.insn	2, 0x
800048c2:	0000                	.insn	2, 0x
800048c4:	4890                	.insn	2, 0x4890
800048c6:	8000                	.insn	2, 0x8000
800048c8:	0001                	.insn	2, 0x0001
800048ca:	0000                	.insn	2, 0x
800048cc:	4890                	.insn	2, 0x4890
800048ce:	8000                	.insn	2, 0x8000
800048d0:	0001                	.insn	2, 0x0001
	...

800048d4 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.222>:
800048d4:	6170                	.insn	2, 0x6170
800048d6:	696e                	.insn	2, 0x696e
800048d8:	64656b63          	bltu	a0,t1,80004f2e <__rust_no_alloc_shim_is_unstable>
800048dc:	6120                	.insn	2, 0x6120
800048de:	2074                	.insn	2, 0x2074

800048e0 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.223>:
800048e0:	0a3a                	.insn	2, 0x0a3a

800048e2 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.250>:
800048e2:	203a                	.insn	2, 0x203a

800048e4 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.251>:
800048e4:	0001                	.insn	2, 0x0001
800048e6:	0000                	.insn	2, 0x
800048e8:	0000                	.insn	2, 0x
800048ea:	0000                	.insn	2, 0x
800048ec:	48e2                	.insn	2, 0x48e2
800048ee:	8000                	.insn	2, 0x8000
800048f0:	0002                	.insn	2, 0x0002
	...

800048f4 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.252>:
800048f4:	0000                	.insn	2, 0x
800048f6:	0000                	.insn	2, 0x
800048f8:	000c                	.insn	2, 0x000c
800048fa:	0000                	.insn	2, 0x
800048fc:	0004                	.insn	2, 0x0004
800048fe:	0000                	.insn	2, 0x
80004900:	2178                	.insn	2, 0x2178
80004902:	8000                	.insn	2, 0x8000
80004904:	2414                	.insn	2, 0x2414
80004906:	8000                	.insn	2, 0x8000
80004908:	2704                	.insn	2, 0x2704
8000490a:	8000                	.insn	2, 0x8000

8000490c <.Lanon.0a795d8d80343cc40e42ade3e02d1552.254>:
8000490c:	7b20                	.insn	2, 0x7b20
8000490e:	                	.insn	2, 0x2c20

8000490f <.Lanon.0a795d8d80343cc40e42ade3e02d1552.255>:
8000490f:	202c                	.insn	2, 0x202c

80004911 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.256>:
80004911:	7b20                	.insn	2, 0x7b20
80004913:	                	.insn	2, 0x2c0a

80004914 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.257>:
80004914:	0a2c                	.insn	2, 0x0a2c

80004916 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.261>:
80004916:	                	.insn	2, 0x207d

80004917 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.262>:
80004917:	7d20                	.insn	2, 0x7d20

80004919 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.289>:
80004919:	7830                	.insn	2, 0x7830

8000491b <.Lanon.0a795d8d80343cc40e42ade3e02d1552.292>:
8000491b:	3030                	.insn	2, 0x3030
8000491d:	3130                	.insn	2, 0x3130
8000491f:	3230                	.insn	2, 0x3230
80004921:	3330                	.insn	2, 0x3330
80004923:	3430                	.insn	2, 0x3430
80004925:	3530                	.insn	2, 0x3530
80004927:	3630                	.insn	2, 0x3630
80004929:	3730                	.insn	2, 0x3730
8000492b:	3830                	.insn	2, 0x3830
8000492d:	3930                	.insn	2, 0x3930
8000492f:	3031                	.insn	2, 0x3031
80004931:	3131                	.insn	2, 0x3131
80004933:	3231                	.insn	2, 0x3231
80004935:	3331                	.insn	2, 0x3331
80004937:	3431                	.insn	2, 0x3431
80004939:	3531                	.insn	2, 0x3531
8000493b:	3631                	.insn	2, 0x3631
8000493d:	3731                	.insn	2, 0x3731
8000493f:	3831                	.insn	2, 0x3831
80004941:	3931                	.insn	2, 0x3931
80004943:	3032                	.insn	2, 0x3032
80004945:	3132                	.insn	2, 0x3132
80004947:	3232                	.insn	2, 0x3232
80004949:	3332                	.insn	2, 0x3332
8000494b:	3432                	.insn	2, 0x3432
8000494d:	3532                	.insn	2, 0x3532
8000494f:	3632                	.insn	2, 0x3632
80004951:	3732                	.insn	2, 0x3732
80004953:	3832                	.insn	2, 0x3832
80004955:	3932                	.insn	2, 0x3932
80004957:	31333033          	.insn	4, 0x31333033
8000495b:	33333233          	.insn	4, 0x33333233
8000495f:	35333433          	.insn	4, 0x35333433
80004963:	37333633          	.insn	4, 0x37333633
80004967:	39333833          	.insn	4, 0x39333833
8000496b:	3034                	.insn	2, 0x3034
8000496d:	3134                	.insn	2, 0x3134
8000496f:	3234                	.insn	2, 0x3234
80004971:	3334                	.insn	2, 0x3334
80004973:	3434                	.insn	2, 0x3434
80004975:	3534                	.insn	2, 0x3534
80004977:	3634                	.insn	2, 0x3634
80004979:	3734                	.insn	2, 0x3734
8000497b:	3834                	.insn	2, 0x3834
8000497d:	3934                	.insn	2, 0x3934
8000497f:	3035                	.insn	2, 0x3035
80004981:	3135                	.insn	2, 0x3135
80004983:	3235                	.insn	2, 0x3235
80004985:	3335                	.insn	2, 0x3335
80004987:	3435                	.insn	2, 0x3435
80004989:	3535                	.insn	2, 0x3535
8000498b:	3635                	.insn	2, 0x3635
8000498d:	3735                	.insn	2, 0x3735
8000498f:	3835                	.insn	2, 0x3835
80004991:	3935                	.insn	2, 0x3935
80004993:	3036                	.insn	2, 0x3036
80004995:	3136                	.insn	2, 0x3136
80004997:	3236                	.insn	2, 0x3236
80004999:	3336                	.insn	2, 0x3336
8000499b:	3436                	.insn	2, 0x3436
8000499d:	3536                	.insn	2, 0x3536
8000499f:	3636                	.insn	2, 0x3636
800049a1:	3736                	.insn	2, 0x3736
800049a3:	3836                	.insn	2, 0x3836
800049a5:	3936                	.insn	2, 0x3936
800049a7:	31373037          	lui	zero,0x31373
800049ab:	33373237          	lui	tp,0x33373
800049af:	35373437          	lui	s0,0x35373
800049b3:	37373637          	lui	a2,0x37373
800049b7:	39373837          	lui	a6,0x39373
800049bb:	3038                	.insn	2, 0x3038
800049bd:	3138                	.insn	2, 0x3138
800049bf:	3238                	.insn	2, 0x3238
800049c1:	3338                	.insn	2, 0x3338
800049c3:	3438                	.insn	2, 0x3438
800049c5:	3538                	.insn	2, 0x3538
800049c7:	3638                	.insn	2, 0x3638
800049c9:	3738                	.insn	2, 0x3738
800049cb:	3838                	.insn	2, 0x3838
800049cd:	3938                	.insn	2, 0x3938
800049cf:	3039                	.insn	2, 0x3039
800049d1:	3139                	.insn	2, 0x3139
800049d3:	3239                	.insn	2, 0x3239
800049d5:	3339                	.insn	2, 0x3339
800049d7:	3439                	.insn	2, 0x3439
800049d9:	3539                	.insn	2, 0x3539
800049db:	3639                	.insn	2, 0x3639
800049dd:	3739                	.insn	2, 0x3739
800049df:	3839                	.insn	2, 0x3839
800049e1:	3939                	.insn	2, 0x3939

800049e3 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.466>:
800049e3:	6f20                	.insn	2, 0x6f20
800049e5:	7475                	.insn	2, 0x7475
800049e7:	6f20                	.insn	2, 0x6f20
800049e9:	2066                	.insn	2, 0x2066
800049eb:	6172                	.insn	2, 0x6172
800049ed:	676e                	.insn	2, 0x676e
800049ef:	2065                	.insn	2, 0x2065
800049f1:	6f66                	.insn	2, 0x6f66
800049f3:	2072                	.insn	2, 0x2072
800049f5:	63696c73          	.insn	4, 0x63696c73
800049f9:	2065                	.insn	2, 0x2065
800049fb:	6c20666f          	jal	a2,8000b0bd <KALLOC_BUFFER+0x50bd>
800049ff:	6e65                	.insn	2, 0x6e65
80004a01:	20687467          	.insn	4, 0x20687467
80004a05:	0000                	.insn	2, 0x
	...

80004a08 <.Lanon.0a795d8d80343cc40e42ade3e02d1552.469>:
80004a08:	4578                	.insn	2, 0x4578
80004a0a:	8000                	.insn	2, 0x8000
80004a0c:	0010                	.insn	2, 0x0010
80004a0e:	0000                	.insn	2, 0x
80004a10:	800049e3          	bltz	zero,80004222 <.Lanon.0967ae7c4fd660b9acdb752d1aeda62f.12+0x26>
80004a14:	0022                	.insn	2, 0x0022
	...

Disassembly of section .eh_frame:

80004a20 <__bss_start-0x4f4>:
80004a20:	0010                	.insn	2, 0x0010
80004a22:	0000                	.insn	2, 0x
80004a24:	0000                	.insn	2, 0x
80004a26:	0000                	.insn	2, 0x
80004a28:	7a01                	.insn	2, 0x7a01
80004a2a:	0052                	.insn	2, 0x0052
80004a2c:	7c01                	.insn	2, 0x7c01
80004a2e:	0101                	.insn	2, 0x0101
80004a30:	00020c1b          	.insn	4, 0x00020c1b
80004a34:	0010                	.insn	2, 0x0010
80004a36:	0000                	.insn	2, 0x
80004a38:	0018                	.insn	2, 0x0018
80004a3a:	0000                	.insn	2, 0x
80004a3c:	d3f4                	.insn	2, 0xd3f4
80004a3e:	ffff                	.insn	2, 0xffff
80004a40:	0008                	.insn	2, 0x0008
80004a42:	0000                	.insn	2, 0x
80004a44:	0000                	.insn	2, 0x
80004a46:	0000                	.insn	2, 0x
80004a48:	001c                	.insn	2, 0x001c
80004a4a:	0000                	.insn	2, 0x
80004a4c:	002c                	.insn	2, 0x002c
80004a4e:	0000                	.insn	2, 0x
80004a50:	d3e8                	.insn	2, 0xd3e8
80004a52:	ffff                	.insn	2, 0xffff
80004a54:	0024                	.insn	2, 0x0024
80004a56:	0000                	.insn	2, 0x
80004a58:	4400                	.insn	2, 0x4400
80004a5a:	100e                	.insn	2, 0x100e
80004a5c:	8148                	.insn	2, 0x8148
80004a5e:	8801                	.insn	2, 0x8801
80004a60:	4402                	.insn	2, 0x4402
80004a62:	080c                	.insn	2, 0x080c
80004a64:	0000                	.insn	2, 0x
80004a66:	0000                	.insn	2, 0x
80004a68:	001c                	.insn	2, 0x001c
80004a6a:	0000                	.insn	2, 0x
80004a6c:	004c                	.insn	2, 0x004c
80004a6e:	0000                	.insn	2, 0x
80004a70:	d3ec                	.insn	2, 0xd3ec
80004a72:	ffff                	.insn	2, 0xffff
80004a74:	00b8                	.insn	2, 0x00b8
80004a76:	0000                	.insn	2, 0x
80004a78:	4400                	.insn	2, 0x4400
80004a7a:	400e                	.insn	2, 0x400e
80004a7c:	8148                	.insn	2, 0x8148
80004a7e:	8801                	.insn	2, 0x8801
80004a80:	4402                	.insn	2, 0x4402
80004a82:	080c                	.insn	2, 0x080c
80004a84:	0000                	.insn	2, 0x
80004a86:	0000                	.insn	2, 0x
80004a88:	001c                	.insn	2, 0x001c
80004a8a:	0000                	.insn	2, 0x
80004a8c:	006c                	.insn	2, 0x006c
80004a8e:	0000                	.insn	2, 0x
80004a90:	d484                	.insn	2, 0xd484
80004a92:	ffff                	.insn	2, 0xffff
80004a94:	0028                	.insn	2, 0x0028
80004a96:	0000                	.insn	2, 0x
80004a98:	4400                	.insn	2, 0x4400
80004a9a:	100e                	.insn	2, 0x100e
80004a9c:	8148                	.insn	2, 0x8148
80004a9e:	8801                	.insn	2, 0x8801
80004aa0:	4402                	.insn	2, 0x4402
80004aa2:	080c                	.insn	2, 0x080c
80004aa4:	0000                	.insn	2, 0x
80004aa6:	0000                	.insn	2, 0x
80004aa8:	0024                	.insn	2, 0x0024
80004aaa:	0000                	.insn	2, 0x
80004aac:	008c                	.insn	2, 0x008c
80004aae:	0000                	.insn	2, 0x
80004ab0:	d48c                	.insn	2, 0xd48c
80004ab2:	ffff                	.insn	2, 0xffff
80004ab4:	014c                	.insn	2, 0x014c
80004ab6:	0000                	.insn	2, 0x
80004ab8:	4400                	.insn	2, 0x4400
80004aba:	500e                	.insn	2, 0x500e
80004abc:	815c                	.insn	2, 0x815c
80004abe:	8801                	.insn	2, 0x8801
80004ac0:	8902                	.insn	2, 0x8902
80004ac2:	93049203          	lh	tp,-1744(s1)
80004ac6:	9405                	.insn	2, 0x9405
80004ac8:	9506                	.insn	2, 0x9506
80004aca:	080c4407          	.insn	4, 0x080c4407
80004ace:	0000                	.insn	2, 0x
80004ad0:	001c                	.insn	2, 0x001c
80004ad2:	0000                	.insn	2, 0x
80004ad4:	00b4                	.insn	2, 0x00b4
80004ad6:	0000                	.insn	2, 0x
80004ad8:	d5b0                	.insn	2, 0xd5b0
80004ada:	ffff                	.insn	2, 0xffff
80004adc:	002c                	.insn	2, 0x002c
80004ade:	0000                	.insn	2, 0x
80004ae0:	4400                	.insn	2, 0x4400
80004ae2:	200e                	.insn	2, 0x200e
80004ae4:	8148                	.insn	2, 0x8148
80004ae6:	8801                	.insn	2, 0x8801
80004ae8:	4402                	.insn	2, 0x4402
80004aea:	080c                	.insn	2, 0x080c
80004aec:	0000                	.insn	2, 0x
80004aee:	0000                	.insn	2, 0x
80004af0:	001c                	.insn	2, 0x001c
80004af2:	0000                	.insn	2, 0x
80004af4:	00d4                	.insn	2, 0x00d4
80004af6:	0000                	.insn	2, 0x
80004af8:	d5bc                	.insn	2, 0xd5bc
80004afa:	ffff                	.insn	2, 0xffff
80004afc:	0048                	.insn	2, 0x0048
80004afe:	0000                	.insn	2, 0x
80004b00:	4400                	.insn	2, 0x4400
80004b02:	300e                	.insn	2, 0x300e
80004b04:	8148                	.insn	2, 0x8148
80004b06:	8801                	.insn	2, 0x8801
80004b08:	4402                	.insn	2, 0x4402
80004b0a:	080c                	.insn	2, 0x080c
80004b0c:	0000                	.insn	2, 0x
80004b0e:	0000                	.insn	2, 0x
80004b10:	001c                	.insn	2, 0x001c
80004b12:	0000                	.insn	2, 0x
80004b14:	00f4                	.insn	2, 0x00f4
80004b16:	0000                	.insn	2, 0x
80004b18:	d5e4                	.insn	2, 0xd5e4
80004b1a:	ffff                	.insn	2, 0xffff
80004b1c:	007c                	.insn	2, 0x007c
80004b1e:	0000                	.insn	2, 0x
80004b20:	4400                	.insn	2, 0x4400
80004b22:	400e                	.insn	2, 0x400e
80004b24:	8148                	.insn	2, 0x8148
80004b26:	8801                	.insn	2, 0x8801
80004b28:	4402                	.insn	2, 0x4402
80004b2a:	080c                	.insn	2, 0x080c
80004b2c:	0000                	.insn	2, 0x
80004b2e:	0000                	.insn	2, 0x
80004b30:	0030                	.insn	2, 0x0030
80004b32:	0000                	.insn	2, 0x
80004b34:	0114                	.insn	2, 0x0114
80004b36:	0000                	.insn	2, 0x
80004b38:	d640                	.insn	2, 0xd640
80004b3a:	ffff                	.insn	2, 0xffff
80004b3c:	029c                	.insn	2, 0x029c
80004b3e:	0000                	.insn	2, 0x
80004b40:	4400                	.insn	2, 0x4400
80004b42:	500e                	.insn	2, 0x500e
80004b44:	8174                	.insn	2, 0x8174
80004b46:	8801                	.insn	2, 0x8801
80004b48:	8902                	.insn	2, 0x8902
80004b4a:	93049203          	lh	tp,-1744(s1)
80004b4e:	9405                	.insn	2, 0x9405
80004b50:	9506                	.insn	2, 0x9506
80004b52:	97089607          	.insn	4, 0x97089607
80004b56:	9809                	.insn	2, 0x9809
80004b58:	990a                	.insn	2, 0x990a
80004b5a:	9b0c9a0b          	.insn	4, 0x9b0c9a0b
80004b5e:	440d                	.insn	2, 0x440d
80004b60:	080c                	.insn	2, 0x080c
80004b62:	0000                	.insn	2, 0x
80004b64:	0024                	.insn	2, 0x0024
80004b66:	0000                	.insn	2, 0x
80004b68:	0148                	.insn	2, 0x0148
80004b6a:	0000                	.insn	2, 0x
80004b6c:	d8a8                	.insn	2, 0xd8a8
80004b6e:	ffff                	.insn	2, 0xffff
80004b70:	00b4                	.insn	2, 0x00b4
80004b72:	0000                	.insn	2, 0x
80004b74:	4400                	.insn	2, 0x4400
80004b76:	200e                	.insn	2, 0x200e
80004b78:	8158                	.insn	2, 0x8158
80004b7a:	8801                	.insn	2, 0x8801
80004b7c:	8902                	.insn	2, 0x8902
80004b7e:	93049203          	lh	tp,-1744(s1)
80004b82:	9405                	.insn	2, 0x9405
80004b84:	4406                	.insn	2, 0x4406
80004b86:	080c                	.insn	2, 0x080c
80004b88:	0000                	.insn	2, 0x
80004b8a:	0000                	.insn	2, 0x
80004b8c:	002c                	.insn	2, 0x002c
80004b8e:	0000                	.insn	2, 0x
80004b90:	0170                	.insn	2, 0x0170
80004b92:	0000                	.insn	2, 0x
80004b94:	d934                	.insn	2, 0xd934
80004b96:	ffff                	.insn	2, 0xffff
80004b98:	023c                	.insn	2, 0x023c
80004b9a:	0000                	.insn	2, 0x
80004b9c:	4400                	.insn	2, 0x4400
80004b9e:	600e                	.insn	2, 0x600e
80004ba0:	8168                	.insn	2, 0x8168
80004ba2:	8801                	.insn	2, 0x8801
80004ba4:	8902                	.insn	2, 0x8902
80004ba6:	93049203          	lh	tp,-1744(s1)
80004baa:	9405                	.insn	2, 0x9405
80004bac:	9506                	.insn	2, 0x9506
80004bae:	97089607          	.insn	4, 0x97089607
80004bb2:	9809                	.insn	2, 0x9809
80004bb4:	440a                	.insn	2, 0x440a
80004bb6:	080c                	.insn	2, 0x080c
80004bb8:	0000                	.insn	2, 0x
80004bba:	0000                	.insn	2, 0x
80004bbc:	001c                	.insn	2, 0x001c
80004bbe:	0000                	.insn	2, 0x
80004bc0:	01a0                	.insn	2, 0x01a0
80004bc2:	0000                	.insn	2, 0x
80004bc4:	db40                	.insn	2, 0xdb40
80004bc6:	ffff                	.insn	2, 0xffff
80004bc8:	0038                	.insn	2, 0x0038
80004bca:	0000                	.insn	2, 0x
80004bcc:	4400                	.insn	2, 0x4400
80004bce:	100e                	.insn	2, 0x100e
80004bd0:	8148                	.insn	2, 0x8148
80004bd2:	8801                	.insn	2, 0x8801
80004bd4:	4402                	.insn	2, 0x4402
80004bd6:	080c                	.insn	2, 0x080c
80004bd8:	0000                	.insn	2, 0x
80004bda:	0000                	.insn	2, 0x
80004bdc:	002c                	.insn	2, 0x002c
80004bde:	0000                	.insn	2, 0x
80004be0:	01c0                	.insn	2, 0x01c0
80004be2:	0000                	.insn	2, 0x
80004be4:	db58                	.insn	2, 0xdb58
80004be6:	ffff                	.insn	2, 0xffff
80004be8:	0264                	.insn	2, 0x0264
80004bea:	0000                	.insn	2, 0x
80004bec:	4400                	.insn	2, 0x4400
80004bee:	500e                	.insn	2, 0x500e
80004bf0:	8168                	.insn	2, 0x8168
80004bf2:	8801                	.insn	2, 0x8801
80004bf4:	8902                	.insn	2, 0x8902
80004bf6:	93049203          	lh	tp,-1744(s1)
80004bfa:	9405                	.insn	2, 0x9405
80004bfc:	9506                	.insn	2, 0x9506
80004bfe:	97089607          	.insn	4, 0x97089607
80004c02:	9809                	.insn	2, 0x9809
80004c04:	440a                	.insn	2, 0x440a
80004c06:	080c                	.insn	2, 0x080c
80004c08:	0000                	.insn	2, 0x
80004c0a:	0000                	.insn	2, 0x
80004c0c:	0030                	.insn	2, 0x0030
80004c0e:	0000                	.insn	2, 0x
80004c10:	01f0                	.insn	2, 0x01f0
80004c12:	0000                	.insn	2, 0x
80004c14:	dd8c                	.insn	2, 0xdd8c
80004c16:	ffff                	.insn	2, 0xffff
80004c18:	0354                	.insn	2, 0x0354
80004c1a:	0000                	.insn	2, 0x
80004c1c:	4400                	.insn	2, 0x4400
80004c1e:	400e                	.insn	2, 0x400e
80004c20:	8174                	.insn	2, 0x8174
80004c22:	8801                	.insn	2, 0x8801
80004c24:	8902                	.insn	2, 0x8902
80004c26:	93049203          	lh	tp,-1744(s1)
80004c2a:	9405                	.insn	2, 0x9405
80004c2c:	9506                	.insn	2, 0x9506
80004c2e:	97089607          	.insn	4, 0x97089607
80004c32:	9809                	.insn	2, 0x9809
80004c34:	990a                	.insn	2, 0x990a
80004c36:	9b0c9a0b          	.insn	4, 0x9b0c9a0b
80004c3a:	440d                	.insn	2, 0x440d
80004c3c:	080c                	.insn	2, 0x080c
80004c3e:	0000                	.insn	2, 0x
80004c40:	0024                	.insn	2, 0x0024
80004c42:	0000                	.insn	2, 0x
80004c44:	0224                	.insn	2, 0x0224
80004c46:	0000                	.insn	2, 0x
80004c48:	e0ac                	.insn	2, 0xe0ac
80004c4a:	ffff                	.insn	2, 0xffff
80004c4c:	00ac                	.insn	2, 0x00ac
80004c4e:	0000                	.insn	2, 0x
80004c50:	4400                	.insn	2, 0x4400
80004c52:	200e                	.insn	2, 0x200e
80004c54:	8158                	.insn	2, 0x8158
80004c56:	8801                	.insn	2, 0x8801
80004c58:	8902                	.insn	2, 0x8902
80004c5a:	93049203          	lh	tp,-1744(s1)
80004c5e:	9405                	.insn	2, 0x9405
80004c60:	4406                	.insn	2, 0x4406
80004c62:	080c                	.insn	2, 0x080c
80004c64:	0000                	.insn	2, 0x
80004c66:	0000                	.insn	2, 0x
80004c68:	0028                	.insn	2, 0x0028
80004c6a:	0000                	.insn	2, 0x
80004c6c:	024c                	.insn	2, 0x024c
80004c6e:	0000                	.insn	2, 0x
80004c70:	e130                	.insn	2, 0xe130
80004c72:	ffff                	.insn	2, 0xffff
80004c74:	0298                	.insn	2, 0x0298
80004c76:	0000                	.insn	2, 0x
80004c78:	4400                	.insn	2, 0x4400
80004c7a:	300e                	.insn	2, 0x300e
80004c7c:	8164                	.insn	2, 0x8164
80004c7e:	8801                	.insn	2, 0x8801
80004c80:	8902                	.insn	2, 0x8902
80004c82:	93049203          	lh	tp,-1744(s1)
80004c86:	9405                	.insn	2, 0x9405
80004c88:	9506                	.insn	2, 0x9506
80004c8a:	97089607          	.insn	4, 0x97089607
80004c8e:	4409                	.insn	2, 0x4409
80004c90:	080c                	.insn	2, 0x080c
80004c92:	0000                	.insn	2, 0x
80004c94:	001c                	.insn	2, 0x001c
80004c96:	0000                	.insn	2, 0x
80004c98:	0278                	.insn	2, 0x0278
80004c9a:	0000                	.insn	2, 0x
80004c9c:	e39c                	.insn	2, 0xe39c
80004c9e:	ffff                	.insn	2, 0xffff
80004ca0:	002c                	.insn	2, 0x002c
80004ca2:	0000                	.insn	2, 0x
80004ca4:	4400                	.insn	2, 0x4400
80004ca6:	100e                	.insn	2, 0x100e
80004ca8:	8148                	.insn	2, 0x8148
80004caa:	8801                	.insn	2, 0x8801
80004cac:	4402                	.insn	2, 0x4402
80004cae:	080c                	.insn	2, 0x080c
80004cb0:	0000                	.insn	2, 0x
80004cb2:	0000                	.insn	2, 0x
80004cb4:	002c                	.insn	2, 0x002c
80004cb6:	0000                	.insn	2, 0x
80004cb8:	0298                	.insn	2, 0x0298
80004cba:	0000                	.insn	2, 0x
80004cbc:	e3a8                	.insn	2, 0xe3a8
80004cbe:	ffff                	.insn	2, 0xffff
80004cc0:	0144                	.insn	2, 0x0144
80004cc2:	0000                	.insn	2, 0x
80004cc4:	4400                	.insn	2, 0x4400
80004cc6:	400e                	.insn	2, 0x400e
80004cc8:	816c                	.insn	2, 0x816c
80004cca:	8801                	.insn	2, 0x8801
80004ccc:	8902                	.insn	2, 0x8902
80004cce:	93049203          	lh	tp,-1744(s1)
80004cd2:	9405                	.insn	2, 0x9405
80004cd4:	9506                	.insn	2, 0x9506
80004cd6:	97089607          	.insn	4, 0x97089607
80004cda:	9809                	.insn	2, 0x9809
80004cdc:	990a                	.insn	2, 0x990a
80004cde:	080c440b          	.insn	4, 0x080c440b
80004ce2:	0000                	.insn	2, 0x
80004ce4:	0024                	.insn	2, 0x0024
80004ce6:	0000                	.insn	2, 0x
80004ce8:	02c8                	.insn	2, 0x02c8
80004cea:	0000                	.insn	2, 0x
80004cec:	e4bc                	.insn	2, 0xe4bc
80004cee:	ffff                	.insn	2, 0xffff
80004cf0:	00e8                	.insn	2, 0x00e8
80004cf2:	0000                	.insn	2, 0x
80004cf4:	4400                	.insn	2, 0x4400
80004cf6:	a00e                	.insn	2, 0xa00e
80004cf8:	5801                	.insn	2, 0x5801
80004cfa:	0181                	.insn	2, 0x0181
80004cfc:	0288                	.insn	2, 0x0288
80004cfe:	0389                	.insn	2, 0x0389
80004d00:	0492                	.insn	2, 0x0492
80004d02:	06940593          	addi	a1,s0,105 # 35373069 <.Lline_table_start2+0x35371cca>
80004d06:	0c44                	.insn	2, 0x0c44
80004d08:	0008                	.insn	2, 0x0008
80004d0a:	0000                	.insn	2, 0x
80004d0c:	001c                	.insn	2, 0x001c
80004d0e:	0000                	.insn	2, 0x
80004d10:	02f0                	.insn	2, 0x02f0
80004d12:	0000                	.insn	2, 0x
80004d14:	e57c                	.insn	2, 0xe57c
80004d16:	ffff                	.insn	2, 0xffff
80004d18:	0018                	.insn	2, 0x0018
80004d1a:	0000                	.insn	2, 0x
80004d1c:	4400                	.insn	2, 0x4400
80004d1e:	100e                	.insn	2, 0x100e
80004d20:	8148                	.insn	2, 0x8148
80004d22:	8801                	.insn	2, 0x8801
80004d24:	4402                	.insn	2, 0x4402
80004d26:	080c                	.insn	2, 0x080c
80004d28:	0000                	.insn	2, 0x
80004d2a:	0000                	.insn	2, 0x
80004d2c:	001c                	.insn	2, 0x001c
80004d2e:	0000                	.insn	2, 0x
80004d30:	0310                	.insn	2, 0x0310
80004d32:	0000                	.insn	2, 0x
80004d34:	e574                	.insn	2, 0xe574
80004d36:	ffff                	.insn	2, 0xffff
80004d38:	0238                	.insn	2, 0x0238
80004d3a:	0000                	.insn	2, 0x
80004d3c:	4400                	.insn	2, 0x4400
80004d3e:	100e                	.insn	2, 0x100e
80004d40:	8148                	.insn	2, 0x8148
80004d42:	8801                	.insn	2, 0x8801
80004d44:	4402                	.insn	2, 0x4402
80004d46:	080c                	.insn	2, 0x080c
80004d48:	0000                	.insn	2, 0x
80004d4a:	0000                	.insn	2, 0x
80004d4c:	001c                	.insn	2, 0x001c
80004d4e:	0000                	.insn	2, 0x
80004d50:	0330                	.insn	2, 0x0330
80004d52:	0000                	.insn	2, 0x
80004d54:	e78c                	.insn	2, 0xe78c
80004d56:	ffff                	.insn	2, 0xffff
80004d58:	0050                	.insn	2, 0x0050
80004d5a:	0000                	.insn	2, 0x
80004d5c:	4400                	.insn	2, 0x4400
80004d5e:	100e                	.insn	2, 0x100e
80004d60:	8148                	.insn	2, 0x8148
80004d62:	8801                	.insn	2, 0x8801
80004d64:	4402                	.insn	2, 0x4402
80004d66:	080c                	.insn	2, 0x080c
80004d68:	0000                	.insn	2, 0x
80004d6a:	0000                	.insn	2, 0x
80004d6c:	001c                	.insn	2, 0x001c
80004d6e:	0000                	.insn	2, 0x
80004d70:	0350                	.insn	2, 0x0350
80004d72:	0000                	.insn	2, 0x
80004d74:	e7bc                	.insn	2, 0xe7bc
80004d76:	ffff                	.insn	2, 0xffff
80004d78:	00f0                	.insn	2, 0x00f0
80004d7a:	0000                	.insn	2, 0x
80004d7c:	4400                	.insn	2, 0x4400
80004d7e:	900e                	.insn	2, 0x900e
80004d80:	4801                	.insn	2, 0x4801
80004d82:	0181                	.insn	2, 0x0181
80004d84:	0288                	.insn	2, 0x0288
80004d86:	0c44                	.insn	2, 0x0c44
80004d88:	0008                	.insn	2, 0x0008
80004d8a:	0000                	.insn	2, 0x
80004d8c:	001c                	.insn	2, 0x001c
80004d8e:	0000                	.insn	2, 0x
80004d90:	0370                	.insn	2, 0x0370
80004d92:	0000                	.insn	2, 0x
80004d94:	e88c                	.insn	2, 0xe88c
80004d96:	ffff                	.insn	2, 0xffff
80004d98:	0084                	.insn	2, 0x0084
80004d9a:	0000                	.insn	2, 0x
80004d9c:	4400                	.insn	2, 0x4400
80004d9e:	900e                	.insn	2, 0x900e
80004da0:	4801                	.insn	2, 0x4801
80004da2:	0181                	.insn	2, 0x0181
80004da4:	0288                	.insn	2, 0x0288
80004da6:	0c44                	.insn	2, 0x0c44
80004da8:	0008                	.insn	2, 0x0008
80004daa:	0000                	.insn	2, 0x
80004dac:	001c                	.insn	2, 0x001c
80004dae:	0000                	.insn	2, 0x
80004db0:	0390                	.insn	2, 0x0390
80004db2:	0000                	.insn	2, 0x
80004db4:	e8f0                	.insn	2, 0xe8f0
80004db6:	ffff                	.insn	2, 0xffff
80004db8:	0084                	.insn	2, 0x0084
80004dba:	0000                	.insn	2, 0x
80004dbc:	4400                	.insn	2, 0x4400
80004dbe:	900e                	.insn	2, 0x900e
80004dc0:	4801                	.insn	2, 0x4801
80004dc2:	0181                	.insn	2, 0x0181
80004dc4:	0288                	.insn	2, 0x0288
80004dc6:	0c44                	.insn	2, 0x0c44
80004dc8:	0008                	.insn	2, 0x0008
80004dca:	0000                	.insn	2, 0x
80004dcc:	001c                	.insn	2, 0x001c
80004dce:	0000                	.insn	2, 0x
80004dd0:	03b0                	.insn	2, 0x03b0
80004dd2:	0000                	.insn	2, 0x
80004dd4:	e954                	.insn	2, 0xe954
80004dd6:	ffff                	.insn	2, 0xffff
80004dd8:	00f8                	.insn	2, 0x00f8
80004dda:	0000                	.insn	2, 0x
80004ddc:	4400                	.insn	2, 0x4400
80004dde:	900e                	.insn	2, 0x900e
80004de0:	4801                	.insn	2, 0x4801
80004de2:	0181                	.insn	2, 0x0181
80004de4:	0288                	.insn	2, 0x0288
80004de6:	0c44                	.insn	2, 0x0c44
80004de8:	0008                	.insn	2, 0x0008
80004dea:	0000                	.insn	2, 0x
80004dec:	001c                	.insn	2, 0x001c
80004dee:	0000                	.insn	2, 0x
80004df0:	03d0                	.insn	2, 0x03d0
80004df2:	0000                	.insn	2, 0x
80004df4:	ea2c                	.insn	2, 0xea2c
80004df6:	ffff                	.insn	2, 0xffff
80004df8:	0030                	.insn	2, 0x0030
80004dfa:	0000                	.insn	2, 0x
80004dfc:	4400                	.insn	2, 0x4400
80004dfe:	100e                	.insn	2, 0x100e
80004e00:	8148                	.insn	2, 0x8148
80004e02:	8801                	.insn	2, 0x8801
80004e04:	4402                	.insn	2, 0x4402
80004e06:	080c                	.insn	2, 0x080c
80004e08:	0000                	.insn	2, 0x
80004e0a:	0000                	.insn	2, 0x
80004e0c:	0020                	.insn	2, 0x0020
80004e0e:	0000                	.insn	2, 0x
80004e10:	03f0                	.insn	2, 0x03f0
80004e12:	0000                	.insn	2, 0x
80004e14:	ea3c                	.insn	2, 0xea3c
80004e16:	ffff                	.insn	2, 0xffff
80004e18:	01c4                	.insn	2, 0x01c4
80004e1a:	0000                	.insn	2, 0x
80004e1c:	4400                	.insn	2, 0x4400
80004e1e:	200e                	.insn	2, 0x200e
80004e20:	8150                	.insn	2, 0x8150
80004e22:	8801                	.insn	2, 0x8801
80004e24:	8902                	.insn	2, 0x8902
80004e26:	44049203          	lh	tp,1088(s1)
80004e2a:	080c                	.insn	2, 0x080c
80004e2c:	0000                	.insn	2, 0x
80004e2e:	0000                	.insn	2, 0x
80004e30:	001c                	.insn	2, 0x001c
80004e32:	0000                	.insn	2, 0x
80004e34:	0414                	.insn	2, 0x0414
80004e36:	0000                	.insn	2, 0x
80004e38:	ebdc                	.insn	2, 0xebdc
80004e3a:	ffff                	.insn	2, 0xffff
80004e3c:	002c                	.insn	2, 0x002c
80004e3e:	0000                	.insn	2, 0x
80004e40:	4400                	.insn	2, 0x4400
80004e42:	100e                	.insn	2, 0x100e
80004e44:	8148                	.insn	2, 0x8148
80004e46:	8801                	.insn	2, 0x8801
80004e48:	4402                	.insn	2, 0x4402
80004e4a:	080c                	.insn	2, 0x080c
80004e4c:	0000                	.insn	2, 0x
80004e4e:	0000                	.insn	2, 0x
80004e50:	001c                	.insn	2, 0x001c
80004e52:	0000                	.insn	2, 0x
80004e54:	0434                	.insn	2, 0x0434
80004e56:	0000                	.insn	2, 0x
80004e58:	ebe8                	.insn	2, 0xebe8
80004e5a:	ffff                	.insn	2, 0xffff
80004e5c:	0034                	.insn	2, 0x0034
80004e5e:	0000                	.insn	2, 0x
80004e60:	4400                	.insn	2, 0x4400
80004e62:	100e                	.insn	2, 0x100e
80004e64:	8148                	.insn	2, 0x8148
80004e66:	8801                	.insn	2, 0x8801
80004e68:	4402                	.insn	2, 0x4402
80004e6a:	080c                	.insn	2, 0x080c
80004e6c:	0000                	.insn	2, 0x
80004e6e:	0000                	.insn	2, 0x
80004e70:	001c                	.insn	2, 0x001c
80004e72:	0000                	.insn	2, 0x
80004e74:	0454                	.insn	2, 0x0454
80004e76:	0000                	.insn	2, 0x
80004e78:	ebfc                	.insn	2, 0xebfc
80004e7a:	ffff                	.insn	2, 0xffff
80004e7c:	006c                	.insn	2, 0x006c
80004e7e:	0000                	.insn	2, 0x
80004e80:	4400                	.insn	2, 0x4400
80004e82:	400e                	.insn	2, 0x400e
80004e84:	8148                	.insn	2, 0x8148
80004e86:	8801                	.insn	2, 0x8801
80004e88:	4402                	.insn	2, 0x4402
80004e8a:	080c                	.insn	2, 0x080c
80004e8c:	0000                	.insn	2, 0x
80004e8e:	0000                	.insn	2, 0x
80004e90:	001c                	.insn	2, 0x001c
80004e92:	0000                	.insn	2, 0x
80004e94:	0474                	.insn	2, 0x0474
80004e96:	0000                	.insn	2, 0x
80004e98:	ec48                	.insn	2, 0xec48
80004e9a:	ffff                	.insn	2, 0xffff
80004e9c:	00a8                	.insn	2, 0x00a8
80004e9e:	0000                	.insn	2, 0x
80004ea0:	4400                	.insn	2, 0x4400
80004ea2:	100e                	.insn	2, 0x100e
80004ea4:	8148                	.insn	2, 0x8148
80004ea6:	8801                	.insn	2, 0x8801
80004ea8:	4402                	.insn	2, 0x4402
80004eaa:	080c                	.insn	2, 0x080c
80004eac:	0000                	.insn	2, 0x
80004eae:	0000                	.insn	2, 0x
80004eb0:	001c                	.insn	2, 0x001c
80004eb2:	0000                	.insn	2, 0x
80004eb4:	0494                	.insn	2, 0x0494
80004eb6:	0000                	.insn	2, 0x
80004eb8:	ecd0                	.insn	2, 0xecd0
80004eba:	ffff                	.insn	2, 0xffff
80004ebc:	0110                	.insn	2, 0x0110
80004ebe:	0000                	.insn	2, 0x
80004ec0:	4400                	.insn	2, 0x4400
80004ec2:	100e                	.insn	2, 0x100e
80004ec4:	8148                	.insn	2, 0x8148
80004ec6:	8801                	.insn	2, 0x8801
80004ec8:	4402                	.insn	2, 0x4402
80004eca:	080c                	.insn	2, 0x080c
80004ecc:	0000                	.insn	2, 0x
80004ece:	0000                	.insn	2, 0x
80004ed0:	001c                	.insn	2, 0x001c
80004ed2:	0000                	.insn	2, 0x
80004ed4:	04b4                	.insn	2, 0x04b4
80004ed6:	0000                	.insn	2, 0x
80004ed8:	edc0                	.insn	2, 0xedc0
80004eda:	ffff                	.insn	2, 0xffff
80004edc:	0024                	.insn	2, 0x0024
80004ede:	0000                	.insn	2, 0x
80004ee0:	4400                	.insn	2, 0x4400
80004ee2:	100e                	.insn	2, 0x100e
80004ee4:	8148                	.insn	2, 0x8148
80004ee6:	8801                	.insn	2, 0x8801
80004ee8:	4402                	.insn	2, 0x4402
80004eea:	080c                	.insn	2, 0x080c
80004eec:	0000                	.insn	2, 0x
80004eee:	0000                	.insn	2, 0x
80004ef0:	001c                	.insn	2, 0x001c
80004ef2:	0000                	.insn	2, 0x
80004ef4:	04d4                	.insn	2, 0x04d4
80004ef6:	0000                	.insn	2, 0x
80004ef8:	edc4                	.insn	2, 0xedc4
80004efa:	ffff                	.insn	2, 0xffff
80004efc:	0214                	.insn	2, 0x0214
80004efe:	0000                	.insn	2, 0x
80004f00:	4400                	.insn	2, 0x4400
80004f02:	100e                	.insn	2, 0x100e
80004f04:	8148                	.insn	2, 0x8148
80004f06:	8801                	.insn	2, 0x8801
80004f08:	4402                	.insn	2, 0x4402
80004f0a:	080c                	.insn	2, 0x080c
	...

Disassembly of section .comment:

00000000 <.comment>:
   0:	4c00                	.insn	2, 0x4c00
   2:	6e69                	.insn	2, 0x6e69
   4:	3a72656b          	.insn	4, 0x3a72656b
   8:	4c20                	.insn	2, 0x4c20
   a:	444c                	.insn	2, 0x444c
   c:	3120                	.insn	2, 0x3120
   e:	2e39                	.insn	2, 0x2e39
  10:	2e31                	.insn	2, 0x2e31
  12:	2034                	.insn	2, 0x2034
  14:	2f28                	.insn	2, 0x2f28
  16:	63656863          	bltu	a0,s6,646 <.Lline_table_start1+0x2f0>
  1a:	74756f6b          	.insn	4, 0x74756f6b
  1e:	6372732f          	.insn	4, 0x6372732f
  22:	766c6c2f          	.insn	4, 0x766c6c2f
  26:	2d6d                	.insn	2, 0x2d6d
  28:	7270                	.insn	2, 0x7270
  2a:	63656a6f          	jal	s4,56660 <.Lline_table_start2+0x552c1>
  2e:	2f74                	.insn	2, 0x2f74
  30:	6c6c                	.insn	2, 0x6c6c
  32:	6d76                	.insn	2, 0x6d76
  34:	3120                	.insn	2, 0x3120
  36:	3430                	.insn	2, 0x3430
  38:	3064                	.insn	2, 0x3064
  3a:	3164                	.insn	2, 0x3164
  3c:	6336                	.insn	2, 0x6336
  3e:	63376333          	.insn	4, 0x63376333
  42:	66656633          	.insn	4, 0x66656633
  46:	3432                	.insn	2, 0x3432
  48:	65663533          	.insn	4, 0x65663533
  4c:	3666                	.insn	2, 0x3666
  4e:	6665                	.insn	2, 0x6665
  50:	3262                	.insn	2, 0x3262
  52:	3564                	.insn	2, 0x3564
  54:	30376237          	lui	tp,0x30376
  58:	6666                	.insn	2, 0x6666
  5a:	3766                	.insn	2, 0x3766
  5c:	72002933          	.insn	4, 0x72002933
  60:	7375                	.insn	2, 0x7375
  62:	6374                	.insn	2, 0x6374
  64:	7620                	.insn	2, 0x7620
  66:	7265                	.insn	2, 0x7265
  68:	6e6f6973          	.insn	4, 0x6e6f6973
  6c:	3120                	.insn	2, 0x3120
  6e:	382e                	.insn	2, 0x382e
  70:	2e35                	.insn	2, 0x2e35
  72:	2d30                	.insn	2, 0x2d30
  74:	696e                	.insn	2, 0x696e
  76:	6c746867          	.insn	4, 0x6c746867
  7a:	2079                	.insn	2, 0x2079
  7c:	3228                	.insn	2, 0x3228
  7e:	6638                	.insn	2, 0x6638
  80:	61623263          	.insn	4, 0x61623263
  84:	32203137          	lui	sp,0x32203
  88:	3230                	.insn	2, 0x3230
  8a:	2d34                	.insn	2, 0x2d34
  8c:	3131                	.insn	2, 0x3131
  8e:	322d                	.insn	2, 0x322d
  90:	2934                	.insn	2, 0x2934
	...

Disassembly of section .riscv.attributes:

00000000 <.riscv.attributes>:
   0:	3341                	.insn	2, 0x3341
   2:	0000                	.insn	2, 0x
   4:	7200                	.insn	2, 0x7200
   6:	7369                	.insn	2, 0x7369
   8:	01007663          	bgeu	zero,a6,14 <.Lline_table_start0+0x14>
   c:	0029                	.insn	2, 0x0029
   e:	0000                	.insn	2, 0x
  10:	1004                	.insn	2, 0x1004
  12:	7205                	.insn	2, 0x7205
  14:	3376                	.insn	2, 0x3376
  16:	6932                	.insn	2, 0x6932
  18:	7032                	.insn	2, 0x7032
  1a:	5f31                	.insn	2, 0x5f31
  1c:	326d                	.insn	2, 0x326d
  1e:	3070                	.insn	2, 0x3070
  20:	7a5f 6369 6f62      	.insn	6, 0x6f6263697a5f
  26:	316d                	.insn	2, 0x316d
  28:	3070                	.insn	2, 0x3070
  2a:	7a5f 6d6d 6c75      	.insn	6, 0x6c756d6d7a5f
  30:	7031                	.insn	2, 0x7031
  32:	0030                	.insn	2, 0x0030
