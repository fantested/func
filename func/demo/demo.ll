; ModuleID = 'FunC'
source_filename = "FunC"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@greet_ptr = global void ()* (i1)* null
@main_ptr = global i32 ()* null
@map_ptr = global i32* (i32 (i32)*, i32, i32*)* null
@reduce_ptr = global i32 (i32 (i32, i32)*, i32, i32*)* null
@twice_ptr = global i32 (i32 (i32)*, i32)* null
@tmp = private unnamed_addr constant [13 x i8] c"How are you?\00"
@tmp.3 = private unnamed_addr constant [11 x i8] c"What's up?\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32* @map(i32 (i32)* %f, i32 %length, i32* %arr) {
entry:
  %f1 = alloca i32 (i32)*
  store i32 (i32)* %f, i32 (i32)** %f1
  %length2 = alloca i32
  store i32 %length, i32* %length2
  %arr3 = alloca i32*
  store i32* %arr, i32** %arr3
  %narr = alloca i32*
  %i = alloca i32
  %length4 = load i32, i32* %length2
  %mallocsize = mul i32 %length4, ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32)
  %malloccall = tail call i8* @malloc(i32 %mallocsize)
  %arrptr = bitcast i8* %malloccall to i32*
  store i32* %arrptr, i32** %narr
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %while_body, %entry
  %i5 = load i32, i32* %i
  %length6 = load i32, i32* %length2
  %tmp = icmp slt i32 %i5, %length6
  br i1 %tmp, label %while_body, label %merge

while_body:                                       ; preds = %while
  %tmp7 = load i32 (i32)*, i32 (i32)** %f1
  %arr8 = load i32*, i32** %arr3
  %i9 = load i32, i32* %i
  %tmp10 = getelementptr i32, i32* %arr8, i32 %i9
  %tmp11 = load i32, i32* %tmp10
  %0 = call i32 %tmp7(i32 %tmp11)
  %narr12 = load i32*, i32** %narr
  %i13 = load i32, i32* %i
  %narr14 = getelementptr i32, i32* %narr12, i32 %i13
  store i32 %0, i32* %narr14
  %i15 = load i32, i32* %i
  %tmp16 = add i32 %i15, 1
  store i32 %tmp16, i32* %i
  br label %while

merge:                                            ; preds = %while
  %narr17 = load i32*, i32** %narr
  ret i32* %narr17
}

define i32 @reduce(i32 (i32, i32)* %f, i32 %length, i32* %arr) {
entry:
  %f1 = alloca i32 (i32, i32)*
  store i32 (i32, i32)* %f, i32 (i32, i32)** %f1
  %length2 = alloca i32
  store i32 %length, i32* %length2
  %arr3 = alloca i32*
  store i32* %arr, i32** %arr3
  %i = alloca i32
  %result = alloca i32
  %length4 = load i32, i32* %length2
  %tmp = icmp sle i32 %length4, 0
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else
  %arr5 = load i32*, i32** %arr3
  %tmp6 = getelementptr i32, i32* %arr5, i32 0
  %tmp7 = load i32, i32* %tmp6
  store i32 %tmp7, i32* %result
  store i32 1, i32* %i
  br label %while

then:                                             ; preds = %entry
  ret i32 0

else:                                             ; preds = %entry
  br label %merge

while:                                            ; preds = %while_body, %merge
  %i8 = load i32, i32* %i
  %length9 = load i32, i32* %length2
  %tmp10 = icmp slt i32 %i8, %length9
  br i1 %tmp10, label %while_body, label %merge19

while_body:                                       ; preds = %while
  %tmp11 = load i32 (i32, i32)*, i32 (i32, i32)** %f1
  %arr12 = load i32*, i32** %arr3
  %i13 = load i32, i32* %i
  %tmp14 = getelementptr i32, i32* %arr12, i32 %i13
  %tmp15 = load i32, i32* %tmp14
  %result16 = load i32, i32* %result
  %0 = call i32 %tmp11(i32 %result16, i32 %tmp15)
  store i32 %0, i32* %result
  %i17 = load i32, i32* %i
  %tmp18 = add i32 %i17, 1
  store i32 %tmp18, i32* %i
  br label %while

merge19:                                          ; preds = %while
  %result20 = load i32, i32* %result
  ret i32 %result20
}

define i32 @main() {
entry:
  store void ()* (i1)* @greet, void ()* (i1)** @greet_ptr
  store i32 ()* @main, i32 ()** @main_ptr
  store i32* (i32 (i32)*, i32, i32*)* @map, i32* (i32 (i32)*, i32, i32*)** @map_ptr
  store i32 (i32 (i32, i32)*, i32, i32*)* @reduce, i32 (i32 (i32, i32)*, i32, i32*)** @reduce_ptr
  store i32 (i32 (i32)*, i32)* @twice, i32 (i32 (i32)*, i32)** @twice_ptr
  %r1 = alloca i32
  %r2 = alloca i32
  %r3 = alloca i32
  %len = alloca i32
  %numbers = alloca i32*
  %squared_numbers = alloca i32*
  %square = alloca i32 (i32)*
  %sum = alloca i32 (i32, i32)*
  %greet_casual = alloca void ()*
  store i32 5, i32* %len
  %malloccall = tail call i8* @malloc(i32 mul (i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32), i32 5))
  %arrptr = bitcast i8* %malloccall to i32*
  %tmp = getelementptr i32, i32* %arrptr, i32 0
  store i32 1, i32* %tmp
  %tmp1 = getelementptr i32, i32* %arrptr, i32 1
  store i32 2, i32* %tmp1
  %tmp2 = getelementptr i32, i32* %arrptr, i32 2
  store i32 3, i32* %tmp2
  %tmp3 = getelementptr i32, i32* %arrptr, i32 3
  store i32 4, i32* %tmp3
  %tmp4 = getelementptr i32, i32* %arrptr, i32 4
  store i32 5, i32* %tmp4
  store i32* %arrptr, i32** %numbers
  store i32 (i32)* @_anon3, i32 (i32)** %square
  store i32 (i32, i32)* @_anon4, i32 (i32, i32)** %sum
  %tmp5 = load i32 (i32 (i32)*, i32)*, i32 (i32 (i32)*, i32)** @twice_ptr
  %square6 = load i32 (i32)*, i32 (i32)** %square
  %0 = call i32 %tmp5(i32 (i32)* %square6, i32 2)
  store i32 %0, i32* %r1
  %r17 = load i32, i32* %r1
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %r17)
  %tmp8 = load void ()* (i1)*, void ()* (i1)** @greet_ptr
  %1 = call void ()* %tmp8(i1 false)
  store void ()* %1, void ()** %greet_casual
  %tmp9 = load void ()*, void ()** %greet_casual
  call void %tmp9()
  %tmp10 = load i32* (i32 (i32)*, i32, i32*)*, i32* (i32 (i32)*, i32, i32*)** @map_ptr
  %numbers11 = load i32*, i32** %numbers
  %len12 = load i32, i32* %len
  %square13 = load i32 (i32)*, i32 (i32)** %square
  %2 = call i32* %tmp10(i32 (i32)* %square13, i32 %len12, i32* %numbers11)
  store i32* %2, i32** %squared_numbers
  %tmp14 = load i32 (i32 (i32, i32)*, i32, i32*)*, i32 (i32 (i32, i32)*, i32, i32*)** @reduce_ptr
  %squared_numbers15 = load i32*, i32** %squared_numbers
  %len16 = load i32, i32* %len
  %sum17 = load i32 (i32, i32)*, i32 (i32, i32)** %sum
  %3 = call i32 %tmp14(i32 (i32, i32)* %sum17, i32 %len16, i32* %squared_numbers15)
  store i32 %3, i32* %r2
  %r218 = load i32, i32* %r2
  %printf19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %r218)
  %tmp20 = load i32 (i32 (i32, i32)*, i32, i32*)*, i32 (i32 (i32, i32)*, i32, i32*)** @reduce_ptr
  %tmp21 = load i32* (i32 (i32)*, i32, i32*)*, i32* (i32 (i32)*, i32, i32*)** @map_ptr
  %numbers22 = load i32*, i32** %numbers
  %len23 = load i32, i32* %len
  %square24 = load i32 (i32)*, i32 (i32)** %square
  %4 = call i32* %tmp21(i32 (i32)* %square24, i32 %len23, i32* %numbers22)
  %len25 = load i32, i32* %len
  %sum26 = load i32 (i32, i32)*, i32 (i32, i32)** %sum
  %5 = call i32 %tmp20(i32 (i32, i32)* %sum26, i32 %len25, i32* %4)
  store i32 %5, i32* %r3
  %r327 = load i32, i32* %r3
  %printf28 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %r327)
  ret i32 0
}

define void ()* @greet(i1 %formal) {
entry:
  %formal1 = alloca i1
  store i1 %formal, i1* %formal1
  %greet_formal = alloca void ()*
  %greet_casual = alloca void ()*
  store void ()* @_anon1, void ()** %greet_formal
  store void ()* @_anon2, void ()** %greet_casual
  %formal2 = load i1, i1* %formal1
  br i1 %formal2, label %then, label %else

merge:                                            ; preds = %else
  %greet_casual4 = load void ()*, void ()** %greet_casual
  ret void ()* %greet_casual4

then:                                             ; preds = %entry
  %greet_formal3 = load void ()*, void ()** %greet_formal
  ret void ()* %greet_formal3

else:                                             ; preds = %entry
  br label %merge
}

define i32 @twice(i32 (i32)* %f, i32 %x) {
entry:
  %f1 = alloca i32 (i32)*
  store i32 (i32)* %f, i32 (i32)** %f1
  %x2 = alloca i32
  store i32 %x, i32* %x2
  %tmp = load i32 (i32)*, i32 (i32)** %f1
  %tmp3 = load i32 (i32)*, i32 (i32)** %f1
  %x4 = load i32, i32* %x2
  %0 = call i32 %tmp3(i32 %x4)
  %1 = call i32 %tmp(i32 %0)
  ret i32 %1
}

define void @_anon1() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i8* getelementptr inbounds ([13 x i8], [13 x i8]* @tmp, i32 0, i32 0))
  ret void
}

define void @_anon2() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i8* getelementptr inbounds ([11 x i8], [11 x i8]* @tmp.3, i32 0, i32 0))
  ret void
}

declare noalias i8* @malloc(i32)

define i32 @_anon3(i32 %a) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %a2 = load i32, i32* %a1
  %a3 = load i32, i32* %a1
  %tmp = mul i32 %a2, %a3
  ret i32 %tmp
}

define i32 @_anon4(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %b2 = alloca i32
  store i32 %b, i32* %b2
  %a3 = load i32, i32* %a1
  %b4 = load i32, i32* %b2
  %tmp = add i32 %a3, %b4
  ret i32 %tmp
}
