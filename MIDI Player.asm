#===========================================================================================
# Description:	Type 0 MIDI player
# Author: 	Omar Darwish
# Date:		04/27/2013
############################################################################################
#Main Subroutine									   #
############################################################################################
.text

main:


jal askForTrack		#get file name
	#notify user that file is being read
	li $v0, 4
	la $a0, _readingFile
	syscall
jal openFile		#open file for reading
jal readHeader		#read file header
jal readTrack		#read file track info
jal arrangeNotes	#arrange notes into memory
jal closeFile		#close file
	#notify uesr that file has loaded successfully
	li $v0, 4
	la $a0, _done
	syscall
	#notify uesr that file is being played
	li $v0, 4
	la $a0, _playingFile
	syscall
jal playEvents		#play loaded events form memory	
	#notify uesr that file has loaded successfully
	li $v0, 4
	la $a0, _done
	syscall
jal playAnother		#ask the user to play another file
j exit

##########################################
#askForTrack: gets file name to be played
#	saves file name in fileName
##########################################	
askForTrack:

li $v0, 54		#inpute dialog 
la $a0, _fileName	#prompt 
la $a1, fileName	#input buffer
li $a2, 122		#Max 120 chars
syscall

li $t0, -2
beq $a1, $t0, exit	#cancel clicked
li $t0, -3
beq $a1, $t0, main #empty input
li $t0, -4
beq $a1, $t0, inputTooLong #input too long

# Setup registers for finding newline character
li 	$t1, 0		# Character index
li	$t2, 10		# Newline character
la	$t3, fileName	# FileName pointer

# Loop through the string to find the newline character
	FindNewLine:
	addi    $t1, $t1, 1     	# Increment character index	
	addi 	$t3, $t3, 1		# Increment FileName pointer
	lb  	$t4, ($t3)     		# Load FileName[$t1] value 	
	bne	$t4, $t2 FindNewLine	# If character != newline, loop
	sb	$0,  fileName($t1)	# Else replace newline with null
	 
jr $ra 
	
	inputTooLong:
	la $a0, _fileNameLength
	li $a1, 0
	li $v0, 55
	syscall
	j main

###############################################
#openFile: opens the specified file to be read
#	If the file cannot be read, prompt user
#	about what to do
###############################################
openFile:

li $v0, 13		#open file
la $a0, fileName	#file name
li $a1, 0		#Read only
li $a2, 0		#Mode ignored
syscall
bltz $v0, openFileError #couldn't read file
move $s0, $v0	#store file descriptor in memory
jr $ra

	openFileError:
	la $a0, _loadTrackError
	li $a1, 0
	li $v0, 55
	syscall
	j playAnother

	midiTypeError:
	la $a0, _midiTypeError
	li $a1, 0
	li $v0, 55
	syscall
	j playAnother

###########################################
#closeFile: closes the recently opened file
###########################################
closeFile:
move $a0, $s0
li $v0, 16
syscall
jr $ra 

#################################################
#playAnother: prompts the user to load a new song
#################################################
playAnother:
li $v0, 62		#turn off all sound
syscall
sw $0, eventOffset
li $v0, 50		#Confirm Dialog
la $a0, _playAnother
syscall
beqz $a0, main		#Yes
j exit			#No, Cancel
	

################################################# 
#readHeader: check header signature. Extract MIDI
#	file type, time division
#	returns Time Division in timeDiv data
#		MIDI type in $t0 (temporary)
#################################################				
readHeader:
subi $sp, $sp, -4	#make room for $ra
sw $ra, 0 ($sp)		#push $ra onto stack

#check for Header signatue
jal readNextWord
li $t0, 0x4D546864	#ASCII for "MThd", Header chunck signature.
bne $v1, $t0, openFileError #Unsupported file type

#check header size
jal readNextWord
li $t0, 0x00000006	#Look for header chunk size
bne $v1, $t0, openFileError #Unsupported file type

#check midi type
jal readNextWord
andi $t0, $v1, 0xFFFF0000
srl $t0, $t0, 16
bne $t0, $zero, midiTypeError #Unsupported midi type

#get time division
jal readNextHalfword
andi $t0, $v1, 0x8000	#mask indicator bit
bne $t0, $zero, midiTypeError	#track uses SMPTE time
andi $v1, $v1, 0x7FFF	#ignore most significant bit
sh $v1, timeDiv		#store time division
lwc1 $f0, tempo		#load tempo into FPU
lwc1 $f1, timeDiv	#load time division into FPU
li $t0, 1000		#
mtc1 $t0, $f2		#move value into fpu
#convert values from word to single precision
cvt.s.w $f0, $f0	#convert tempo
cvt.s.w $f1, $f1	#convert time division
cvt.s.w $f2, $f2	#convert 1000
#carry out conversion
div.s $f0, $f0, $f1	#Tempo/timeDiv
div.s $f0, $f0, $f2	#(Tempo/timeDiv)/1000
swc1 $f0, millisPerDelta#save the calculated milliseconds per delta
lw $ra, 0($sp)		#recall return address
addi $sp, $sp, 4	#pop stack
jr $ra


	####################################################
	# readNextWord: reads next 4 byte word from file
	#	returns word in $v1
	####################################################
	readNextWord:	
	li $t0, 4		#set counter
	li $v1, 0
		readNextByteLoop:
		subi $sp, $sp, 4	#make room for $ra
		sw $ra, 0 ($sp)		#push $ra onto stack	
		jal readNextByte	#get next byte
		lw $ra, 0($sp)		#recall return address
		addi $sp, $sp, 4	#pop stack			
		sll $v1, $v1, 8		#make room for next byte
		or $v1, $v1, $v0	#add to word being built
		subi $t0, $t0, 1	#decrement counter
		bgtz $t0, readNextByteLoop  #loop until 4 byte read
	jr $ra
	####################################################
	# readNextHalfword: reads next 4 byte word from file
	#	returns halfword in $v1
	####################################################
	readNextHalfword:	
	li $t0, 2		#set counter
	li $v1, 0
	j readNextByteLoop	#read requested bytes

	##########################################
	#skipBytes: skips n upcoming bytes in file
	#	$a0	number of bytes to skip
	#
	##########################################
	skipBytes:
	move $s3, $a0 
	subi $sp, $sp, 4	#make room in stack
	sw $ra, 0 ($sp)		#push $ra
	skipBytesLoop:	
	jal readNextByte	
	subi $s3, $s3, 1	#decrement counter
	bgtz $s3, skipBytesLoop
	lw $ra, 0($sp)		#recall return
	addi $sp, $sp, 4	#pop stack
	jr $ra
	#########################################
	#readNextByte: reads next byte from file
	#	returns byte read in $v0
	#########################################
	readNextByte:
	li $v0, 14		#read from file	
	move $a0, $s0		#pass file descriptor
	la $a1, buffer
	li $a2, 1		#read 1 byte
	syscall
	lbu $v0, buffer		#put byte in register 
	jr $ra
	




########################################
#readTrack: checks track chunk signature
########################################
readTrack:
#save return address
subi $sp, $sp, -4	#make room for $ra
sw $ra, 0 ($sp)		#push $ra onto stack

#look for track chunk header
jal readNextWord
li $t0, 0x4D54726B	 #Look for track signature
bne $v1, $t0, openFileError #Unsupported file type

jal readNextWord	#skip track size
lw $ra, 0($sp)		#recall return address
addi $sp, $sp, 4	#pop stack
#return to main
jr $ra

#####################################################
#readDelta: returns value of variable length quantity
#	In the case of MIDI files, Delta Time is
#	a variable length quantity
#	stores in delta var
#	returns quantity in $v0
#####################################################	
	readDelta:
	subi $sp, $sp, 4	#push stack
	sw $ra, 0 ($sp)		#save $ra
	li $t0, 0		#init result register
		readDeltaLoop:
		jal readNextByte
		sll $t0, $t0, 7		#make room for next byte
		andi $t1, $v0, 0x7F	#Mask bits 0-6
		or $t0, $t0, $t1	#combine bytes
		andi $t1, $v0, 0x80	#Mask bit 7
		bgtz $t1, readDeltaLoop #Check if last byte
	move $v0, $t0		#put result in return
	sw $v0, delta		#store in memory
	lw $ra, 0($sp)		#recall return address
	addi $sp, $sp, 4	#pop stack
	jr $ra

arrangeNotes:
subi $sp, $sp, 4	#push stack
sw $ra, 0($sp)		#save $ra
jal nextEvent
lw $ra, 0($sp)		#recall return address
addi $sp, $sp, 4	#pop stack
jr $ra			#return to main
	
	nextEvent:
	subi $sp, $sp, 4	#push stack
	sw $ra, 0($sp)		#save $ra
	jal readDelta		#get next delta time
	jal readNextByte	#get following byte
	lw $ra, 0($sp)		#recall return
	addi $sp, $sp, 4	#pop stack	
	
	#check byte
	li $t0, 0x80		#min status byte
	blt $v0, $t0, nonStatus #not a status byte
	li $t0, 0x7FFFFFFF	#max positive (is a status byte/is supported status)
	sw $t0, statusFlag	#assume that it is a supported status
	sw $t0, isStatus	#isStatus = true
	sb $v0, lastStatus	#save status byte
	
	#check what kind of status
	statusChecking:
	li $t0, 0xFF		#Meta event
	beq $v0, $t0, metaEvent
	
	li $t0, 0xF0		#SysEx Event, FF (meta) already handled
	beq $v0, $t0, sysExEvent

	li $t0, 0xC0		#Program Change
	andi $t1, $v0, 0xF0  	#Mask event bits
	beq $t1, $t0, pgmChange		
	
	li $t0, 0x80		#Note off
	andi $t1, $v0, 0xF0  	#Mask event bits
	beq $t1, $t0, noteOff
	
	li $t0, 0x90		#Note On
	andi $t1, $v0, 0xF0  	#Mask event bits
	beq $t1, $t0, noteOn
	
	#unsupported status
	subi $sp, $sp, 4	#push stack
	sw $ra, 0($sp)		#save $ra
	jal statusSkip		#skip upcoming parameter(s)
	li $t0, 0x80000000	#unsupported status flag
	sw $t0, statusFlag 	#save flag
	lw $ra, 0($sp)		#recall return
	addi $sp, $sp, 4	#pop stack
	j nextEvent
		
		#################################################
		#statusRead: returns paramaters for current event
		#	returns parameter halfword in $s1
		#	returns current status byte in $s3
		#################################################
		statusRead:
		subi $sp, $sp, 4	#push stack
		sw $ra, 0($sp)		#save $ra
		lw $s3, lastStatus	#retrieve previous status byte	
		lw $t0, isStatus	#load bool	
		bltz $t0, notStatusByteRead	#current byte not status (1 param already read)
		move $s3, $v0		#save current status byte		
		jal readNextHalfword	#have not read any params, read 2 params
		move $s1, $v1
		lw $ra, 0($sp)		#recall return
		addi $sp, $sp, 4	#pop stack
		jr $ra
			notStatusByteRead:
			lbu $s1, buffer		#save current byte (param 1)					
			jal readNextByte	#already read 1 param, read 1 more
			sll $s1, $s1, 8		#make room for append
			or $s1, $s1, $v0	#append byte
			lw $ra, 0($sp)		#recall return
			addi $sp, $sp, 4	#pop stack
			jr $ra

		statusSkip:
		subi $sp, $sp, 4	#push stack
		sw $ra, 0($sp)		#save $ra
		lw $t0, isStatus	#load bool	
		bltz $t0, notStatusByte	#current byte not status
		li $a0, 2		#have not read any params, skip 2 upcoming params
		jal skipBytes
		lw $ra, 0($sp)		#recall return
		addi $sp, $sp, 4	#pop stack
		jr $ra
			notStatusByte:
			li $a0, 1		#already read param 1, so skip only 1 param
			jal skipBytes
			lw $ra, 0($sp)		#recall return
			addi $sp, $sp, 4	#pop stack
			jr $ra

			
		
	
		nonStatus:
		li $t0, 0x80000000	#max negative number
		sw $t0, isStatus	#indicate that this byte is not a status byte
		lw $t0, statusFlag	#get status flag
		bgtz $t0, supportedStatus #handle supported status
		#status was not supported
		subi $sp, $sp, 4	#push stack
		sw $ra, 0($sp)		#save $ra
		jal readNextByte	#skip next byte
		lw $ra, 0($sp)		#recall return
		addi $sp, $sp, 4	#pop stack
		j nextEvent
			
			supportedStatus:
			lw $v0, lastStatus
			j statusChecking

		metaEvent: # calls jr $ra (end of track)
		subi $sp, $sp, 4	#push stack
		sw $ra, 0($sp)		#save $ra		
		jal readNextByte	#read type byte
		#check meta event type
		li $t0, 0x51		#tempo event
		beq $v0, $t0, setTempo

		li $t0, 0x2f		#end of track
		beq $v0, $t0, endOfTrack
		
		#unsupported meta event
		jal readDelta		#read length (bytes to skip
		move $a0, $v0		#pass length as argument
		jal skipBytes		#skip bytes
		lw $ra, 0($sp)		#recall return
		addi $sp, $sp, 4	#pop stack
		j nextEvent

			###########################################
			#setTempo: stores tempo change events
			#	saves event to playback queue:
			#	word	variable	bitmask
			# 	word 1:
			#		type (4)	#0xF0000000
			#		delay (ticks)	#0x0FFFFFFF
			#	word 2:
			#		tempo		#0x00FFFFFF
			###########################################
			setTempo:
			subi $sp, $sp, 4	#push stack
			sw $ra, 0($sp)		#store $ra
			#build tempo change word 1
			li $s2, 4		#set type
			sll $s2, $s2, 28	#left shift 7 nibbles
			lw $t0, delta		#load last delta
			or $s2, $s2, $t0	#append delta to type
			#write word 1 to array
			lw $t0, eventOffset	#get current offset
			sw $s2, notes($t0)	#store word 1
			addi $t0, $t0, 4	#increment offset by 1 word
			#build tempo change word 2
			li $s2, 0		#clear var
			jal readNextByte	#skip length byte
			jal readNextByte	#read most significant byte
			andi $v0, $v0, 0xFF	#mask first byte
			move $s2, $v0		#add to var
			sll $s2, $s2, 8		#make room for next byte
			jal readNextByte	#read middle byte
			andi $v0, $v0, 0xFF	#mask first byte
			or $s2, $s2, $v0	#add to var
			sll $s2, $s2, 8		#make room for last byte
			jal readNextByte	#read least significant byte
			andi $v0, $v0, 0xFF	#mask first byte
			or $s2, $s2, $v0	#add to var		
			#write word 2 to array
			sw $s2, notes($t0)	#store word 2
			addi $t0, $t0, 4	#increment offset by 1 word
			sw $t0, eventOffset	#store new offset		
			lw $ra, 0($sp)		#recall return
			addi $sp, $sp, 4	#pop stack
			j nextEvent
			
			###########################################
			#endOfTrack: stores end of track event
			#	saves event to playback queue:
			#	word	variable	bitmask
			# 	word 1:
			#		type (5)	#0xF0000000
			#		delay (ticks)	#0x0FFFFFFF
			#	word 2:
			#		0
			###########################################
			endOfTrack:
			#build end of track event word 1
			li $s2, 5		#set type
			sll $s2, $s2, 28	#left shift 7 nibbles
			lw $t0, delta		#load last delta
			or $s2, $s2, $t0	#append delta to type
			#write word 1 to array
			lw $t0, eventOffset	#get current offset
			sw $t0, endOfTrackOffset #mark the position of the end of track
			sw $s2, notes($t0)	#store word 1
			addi $t0, $t0, 4	#increment offset by 1 word
			#write empty word 2 to array
			sw $zero, notes($t0)	#store empty word
			lw $ra, 0($sp)		#recall return
			addi $sp, $sp, 4	#pop stack
			jr $ra			#done with arranging notes
		
		##########################################	
		#sysExEvent: skips System Exclusive events
		##########################################
		sysExEvent:
		subi $sp, $sp, 4	#push stack
		sw $ra, 0($sp)		#save $ra
		jal readDelta		#read length
		addi $v0, $v0, 1	#include 0xF7 bookend byte
		move $a0, $v0		#skip counted bytes
		jal skipBytes
		j nextEvent
		
		###########################################
		#pgmChange: stores program change events
		#	saves event to playback queue:
		#	word	variable	bitmask
		# 	word 1:
		#		type (3)	#0xF0000000
		#		delay (ticks)	#0x0FFFFFFF
		#	word 2:
		#		channel		#0x0000000F
		#		program		#0x00000FF0
		###########################################
		pgmChange:
		subi $sp, $sp, 4	#push stack
		sw $ra, 0($sp)		#store $ra
		li $t0, 0x7FFFFFFF	#max positive (is a status byte/is supported status)
		sw $t0, statusFlag	#assume that it is a supported status
		move $s3, $v0		#save current status byte
		li $s2, 0		#init variable 
		#build program change word 1
		li $s2, 3		#set type
		sll $s2, $s2, 28	#left shift 7 nibbles
		lw $t0, delta		#load last delta
		or $s2, $s2, $t0	#append delta to type
		#write word 1 to array
		lw $t0, eventOffset	#get current offset
		sw $s2, notes($t0)	#store word 1
		addi $t0, $t0, 4	#increment offset by 1 word
		#build program change word 2
		li $s2, 0		#clear variable		
		andi $t1, $s3, 0xF	#mask channel nibble
		or $s2, $t1, $s2	#append channel to var
		jal readNextByte	#get instrument number
		sll $v0, $v0, 4		#shit 1 nibble
		or $s2, $s2, $v0	#append instrument number
		#write word 2 to array
		sw $s2, notes($t0)	#store word 2
		addi $t0, $t0, 4	#increment offset by 1 word
		sw $t0, eventOffset	#store new offset
		lw $ra, 0($sp)		#recall return
		addi $sp, $sp, 4	#pop stack
		j nextEvent
		
		###########################################
		#noteOff: stores note off events
		#	saves event to playback queue:
		#	word	variable	bitmask
		# 	word 1:
		#		type (2)	#0xF0000000
		#		delay (ticks)	#0x0FFFFFFF
		#	word 2:
		#		channel		#0x0000000F
		#		note		#0x00000FF0
		#		velocity	#0x000FF000
		###########################################
		noteOff:		
		#handle note Off
		li $t0, 0x7FFFFFFF	#max positive (is a status byte/is supported status)
		sw $t0, statusFlag	#assume that it is a supported status
		subi $sp, $sp, 4	#push stack
		sw $ra, 0($sp)		#store $ra
		jal statusRead		#get paramaters
		noteOffCont:
		#build note off word 1
		li $s2, 2		#set type
		sll $s2, $s2, 28	#left shift 7 nibbles
		lw $t0, delta		#load last delta
		or $s2, $s2, $t0	#append delta to type
		#write word 1 to array
		lw $t0, eventOffset	#get current offset
		sw $s2, notes($t0)	#store word 1
		addi $t0, $t0, 4	#increment offset by 1 word
		#build note off word 2
		li $s2, 0		#clear variable
		andi $t1, $s3, 0xF	#mask channel nibble
		or $s2, $s2, $t1	#append channel nibble
		andi $t1, $s1, 0xFF00	#get note (param 2)
		srl $t1, $t1, 4		#move right one nibble
		or $s2, $s2, $t1	#append note byte
		andi $t1, $s1, 0xFF	#get volume (param 2)
		sll $t1, $t1, 12	#shift left 3 nibbles
		or $s2, $s2, $t1	#append velocity
		#write word 2 to array
		sw $s2, notes($t0)	#store word 2
		addi $t0, $t0, 4	#increment offset by 1 word
		sw $t0, eventOffset	#store new offset
		lw $ra, 0($sp)		#recall return
		addi $sp, $sp, 4	#pop stack
		j nextEvent
			
			
			###########################################################################
			#findPreviousNote: finds the offset of word 1 of the previous note on event
			#	args:
			#		$a0	offset of word 1 of current note
			#	returns:
			#		$v0	offset of word 1 of previous note
			#			negative if note note found
			###########################################################################
			findPreviousNote:
			subi $a0, $a0, 12	#set offset to word 1 of previous event
			bltz $a0, findPreviousNoteExit	#reached bottom of queue
			lw $t0, notes($a0)	#load event word 1
			li $t1, 1		#set type to 1
			andi $t0, $t0, 0xF	#mask type bits
			bne $t0, $t1, findPreviousNote	#not a note on event, keep looking
			#found
			move $v0, $a0		#move to return register			
			jr $ra
			findPreviousNoteExit:
			li $v0, -1		#reached bottom of stack, no event found
			jr $ra

			###########################################################################
			#findNextNote: finds the offset of word 1 of the next note on event
			#	args:
			#		$a0	offset of word 1 of current note
			#	returns:
			#		$v0	offset of word 1 of next note
			#			negative if note note found
			###########################################################################
			findNextNote:
			addi $a0, $a0, 12	#set offset to word 1 of previous event
			#lw $t0, lastNote	#load offset of last event in queue
			bgt $a0, $t0, findNextNoteExit
			lw $t0, notes($a0)	#load event word 1
			li $t1, 1		#set type to 1
			andi $t0, $t0, 0xF	#mask type bits
			bne $t0, $t1, findNextNote	#not a note on event, keep looking
			#found
			move $v0, $a0		#move to return register			
			jr $ra
			findNextNoteExit:
			li $v0, -1		#reached passed last event, no event found
			jr $ra	

		###########################################
		#noteOn: stores note on events
		#	saves event to playback queue:
		#	word	variable	bitmask
		# 	word 1:
		#		type (1)	#0xF0000000
		#		delay (ticks)	#0x0FFFFFFF
		#	word 2:
		#		channel		#0x0000000F
		#		note		#0x00000FF0
		#		volume		#0x000FF000
		###########################################
		noteOn:		
		#handle note on
		li $t0, 0x7FFFFFFF	#max positive (is a status byte/is supported status)
		sw $t0, statusFlag	#assume that it is a supported status
		subi $sp, $sp, 4	#push stack
		sw $ra, 0($sp)		#store $ra
		jal statusRead		#get paramaters
		#check if actually note off (volume 0)
		andi $t0, $s1, 0xFF	#get volume (param 2)
		beqz $t0, noteOffCont	#note was actually note off (volume 0)
		#build note on word 1
		li $s2, 1		#set type
		sll $s2, $s2, 28	#left shift 7 nibbles
		lw $t0, delta		#load last delta
		or $s2, $s2, $t0	#append delta to type
		#write word 1 to array
		lw $t0, eventOffset	#get current offset
		sw $s2, notes($t0)	#store word 1
		addi $t0, $t0, 4	#increment offset by 1 word
		#build note on word 2
		li $s2, 0		#clear variable
		andi $t1, $s3, 0xF	#mask channel nibble
		or $s2, $s2, $t1	#append channel nibble
		andi $t1, $s1, 0xFF00	#get note (param 2)
		srl $t1, $t1, 4		#move right one nibble
		or $s2, $s2, $t1	#append note byte
		andi $t1, $s1, 0xFF	#get volume (param 2)
		sll $t1, $t1, 12	#shift left 3 nibbles
		or $s2, $s2, $t1	#append volume
		#write word 2 to array
		sw $s2, notes($t0)	#store word 2
		addi $t0, $t0, 4	#increment offset by 1 word
		sw $t0, eventOffset	#store new offset
		lw $ra, 0($sp)		#recall return
		addi $sp, $sp, 4	#pop stack 
		j nextEvent		
#################################################
#playEvents: plays the events stored in the array
#################################################
playEvents:
li $v0, 62		#load NOTE OFF ALL event
syscall
sw $zero, eventOffset		#start at the beginning
playNextEvent:
#check for Keyboard IO
lw $t0, recv_ctrl	#load mapped address
lw $t0, ($t0)		#load from mapped address
bnez $t0, handleIO		#a keyevent has occured

#check if paused
lw $t0, paused
bgtz $t0, playNextEvent	#loop back until it is unpaused

#continue with event playback
lw $t0, eventOffset		#get noteOFfset
lw $t1, notes($t0)		#load word at offset
#calculate delay
andi $t1, $t1, 0x0FFFFFFF #maske delta
mtc1 $t1, $f0		#move delta to fpu
cvt.s.w $f0, $f0	#convert form word to single precision
lwc1 $f2, millisPerDelta #load calculated milliseconds per delta
mul.s $f0, $f0, $f2	#multiply delta by milliseconds per delta
cvt.w.s $f0, $f0	#convert from single precision to word
mfc1 $a0, $f0		#get result from fpu and pass to delay
li $v0, 32		#load delay syscall
syscall

#determine event type
lw $t1, notes($t0)		#load first word again
andi $t1, $t1, 0xF0000000	#mask type bits
srl $t1, $t1, 28		#shift right 7 nibbles

addi $t0, $t0, 4		#get offset ready to read word 2 of event
#Note on event
li $v0, 1
beq $v0, $t1, handleNoteOn
#Note off event
li $v0, 2
beq $v0, $t1, handleNoteOff
#Program change event
li $v0, 3
beq $v0, $t1, handlePgmChange
#Set tempo event
li $v0, 4
beq $v0, $t1, handleTempo
#End of track event
li $v0, 5
beq $v0, $t1, handleEnd

	
	handleNoteOn:	
	#read note on info
	lw $t1, notes($t0)	#load word 2
	andi $a0, $t1, 0x0000000F #mask channel, pass as argument
	andi $a1, $t1, 0x00000FF0 #mask note, pass as argument
	srl $a1, $a1, 4		#shift right one nibble
	andi $a2, $t1, 0x000FF000 #mask volume
	srl $a2, $a2, 12	#shift right 3 nibbles
	li $v0, 60		#load NOTE ON syscall
	syscall
	#increment and store offset
	addi $t0, $t0, 4	#point to next event
	sw $t0, eventOffset	#sotre new offset
	j playNextEvent
	
	handleNoteOff:	
	#read note on info
	lw $t1, notes($t0)	#load word 2
	andi $a0, $t1, 0x0000000F #mask channel, pass as argument
	andi $a1, $t1, 0x00000FF0 #mask note, pass as argument
	srl $a1, $a1, 4		#shift right one nibble
	andi $a2, $t1, 0x000FF000 #mask velocity, pass as argument
	srl $a2, $a2, 12	#shift right 3 nibbles
	li $v0, 61		#load NOTE ON syscall
	syscall
	#increment and store offset
	addi $t0, $t0, 4	#point to next event
	sw $t0, eventOffset	#sotre new offset
	j playNextEvent

	handleTempo:
	#get tempo
	lw $t1, notes ($t0)	#load word 2
	mtc1 $t1, $f0		#move tempo to fpu
	lwc1 $f1, timeDiv	#load time division in fpu
	li $t1, 1000
	mtc1 $t1, $f2		#move 1000 to fpu
	#convert values to single precision
	cvt.s.w $f0, $f0	#convert tempo
	cvt.s.w $f1, $f1	#convert time division
	cvt.s.w $f2, $f2	#convert 1000
	#do calculations
	div.s $f0, $f0, $f1	#fpu divide tempo by time division
	div.s $f0, $f0, $f2	#divide by 1000 to convert microseconds to milliseconds
	swc1 $f0, millisPerDelta #save new milliseconds per delta
	addi $t0, $t0, 4	#point at next event
	sw $t0, eventOffset	#store new offset
	j playNextEvent

	handlePgmChange:
	lw $t1, notes($t0)	#load word 2
	andi $a0, $t1, 0xF	#mask channel
	andi $a1, $t1, 0xFF0	#maske program
	srl $a1, $a1, 4		#shift right one nibble
	li $v0, 38		#load program change syscall
	syscall
	addi $t0, $t0, 4	#point to next event
	sw $t0, eventOffset	#store new offset
	j playNextEvent

	handleEnd:
	#no data to read for end of track, return method
	jr $ra
#####################################
#HandleIO: handles keyboard events
#	supports pause/unpause
#		rewind 10 percent
#		skip ahead 10 percent
#####################################
handleIO:
lw $t0, recv_data	#load mapped address
lw $t0, ($t0)		#load from mapped address

#check if quit
beq $t0, 'q', playAnother
beq $t0, 'Q', playAnother

#check if pause
beq $t0, 'p', pause
beq $t0, 'P', pause

#check if fastForward
beq $t0, 'f', skip
beq $t0, 'F', skip

#check if rewind
beq $t0, 'r', rewind
beq $t0, 'R', rewind
j playNextEvent

	##############################
	#pause: pause/unpause playback
	##############################
	pause:
	#silence ongoing tones
	li $v0, 62
	syscall
	lw $t0, paused		#get pause status
	bgtz $t0, unpause	#if paused, unpause
	#notify user that pause is occuring
	li $v0, 4
	la $a0, _paused
	syscall
	li $t0, 1
	sw $t0, paused
	j playNextEvent
	unpause:
	#notify user that unpause is occuring
	li $v0, 4
	la $a0, _resumed
	syscall
	li $t0, -1
	sw $t0, paused
	j playNextEvent
	
	#############################################
	#skip: skip ahead 10 percent, applying tempos 
	#	along the way
	#############################################
	skip:
	#silence ongoing tones
	li $v0, 62
	syscall
	#notify user that skip is occuring
	li $v0, 4
	la $a0, _skip
	syscall
	#calculate what 10 percent of track is
	lw $t0, endOfTrackOffset#load position of end of track
	li $t1, 10
	div $t0, $t1		#divide by 10
	#find nearest multiple of 8
	mflo $t0		#get whole number, ignore remainder
	li $t1, 8		
	div $t0, $t1		#divide by 8
	mflo $t0		#this is how many events can fit in this 10 percent chunk
	mul $t0, $t1, $t0	#multiply number of events by 8 to get delta size
	sw $t0, skipSize	#save calculated deltaSize
	lw $t1, eventOffset	#get current offset
	lw $t2, endOfTrackOffset #get last event position
	add $t0, $t0, $t1	#current offset + skip size
	sgt $t1, $t0, $t2	#if new position > end of track
	bgtz $t1, greaterThanEnd #new position is greater than end
	sw $t0, nextOffset	#new position < end
	j skipTempos		#apply all skipped over tempos

		greaterThanEnd:
		#set next offset to end of track
		sw $t2, nextOffset
		j skipTempos #apply all skipped over tempos

		skipTemposLoop:
		lw $t0, eventOffset	#get current offset
		addi $t0, $t0, 8	#point to next event
		sw $t0, eventOffset	#store new offset
		lw $t1, nextOffset	#get position of where we are skipping to
		beq $t0, $t1, playNextEvent #reached where we want to be, done. Go to next event
		#apply all offsets from current offset to skip offset
		skipTempos:
		lw $t0, eventOffset	#get current offset
		lw $t1, notes($t0)	#load word 1
		andi $t1, $t1, 0xF0000000#mask type bits
		srl $t1, $t1, 28	#shift right 7 nibbles
		li $t2, 4		#only interested in tempo events
		bne $t2, $t1, skipTemposLoop #not a tempo event
		addi $t0, $t0, 4	#point to word 2
		#get tempo
		lw $t1, notes ($t0)	#load word 2
		mtc1 $t1, $f0		#move tempo to fpu
		lwc1 $f1, timeDiv	#load time division in fpu
		li $t1, 1000
		mtc1 $t1, $f2		#move 1000 to fpu
		#convert values to single precision
		cvt.s.w $f0, $f0	#convert tempo
		cvt.s.w $f1, $f1	#convert time division
		cvt.s.w $f2, $f2	#convert 1000
		#do calculations
		div.s $f0, $f0, $f1	#fpu divide tempo by time division
		div.s $f0, $f0, $f2	#divide by 1000 to convert microseconds to milliseconds
		swc1 $f0, millisPerDelta #save new milliseconds per delta
		j skipTemposLoop
	
	rewind:
	#silence ongoing tones
	li $v0, 62
	syscall
	#notify user that rewind is occuring
	li $v0, 4
	la $a0, _rewind
	syscall
	#calculate what 10 percent of track is
	lw $t0, endOfTrackOffset#load position of end of track
	li $t1, 10
	div $t0, $t1		#divide by 10
	#find nearest multiple of 8
	mflo $t0		#get whole number, ignore remainder
	li $t1, 8		
	div $t0, $t1		#divide by 8
	mflo $t0		#this is how many events can fit in this 10 percent chunk
	mul $t0, $t1, $t0	#multiply number of events by 8 to get delta size
	sw $t0, skipSize	#save calculated deltaSize
	lw $t1, eventOffset	#get current offset
	sub $t0, $t1, $t0	#current offset - skip size
	slt $t1, $t0, $0	#if new position < start
	bgtz $t1, lessThanStart #new position is less than start
	sw $t0, nextOffset	#new position > start	
	j applyLastTempo	#apply last tempo preceeding nextOffset
		lessThanStart:
		sw $0, nextOffset	#set offset to the beginning of track
		sw $0, eventOffset	#set playback at beginning of track
		j playNextEvent
				
		applyLastTempo:
		lw $t0, nextOffset
		applyLastTempoLoop:
		#silence ongoing tones
		li $v0, 62
		syscall
		subi $t0, $t0, 8	#go back one event
		blez $t0, noTempoFound	#reached beginning of track
		lw $t1, notes($t0)	#load word 1
		andi $t1, $t1, 0xF0000000#mask type bits
		srl $t1, $t1, 28	#shift right 7 nibbles
		li $t2, 4		#only interested in tempo events
		bne $t2, $t1, applyLastTempoLoop #not a tempo event
		addi $t1, $0, 4		#point at word 2
		#load tempo
		lw $t1, notes($t1)	#load word 2
		#get tempo
		mtc1 $t1, $f0		#move tempo to fpu
		lwc1 $f1, timeDiv	#load time division in fpu
		li $t1, 1000
		mtc1 $t1, $f2		#move 1000 to fpu
		#convert values to single precision
		cvt.s.w $f0, $f0	#convert tempo
		cvt.s.w $f1, $f1	#convert time division
		cvt.s.w $f2, $f2	#convert 1000
		#do calculations
		div.s $f0, $f0, $f1	#fpu divide tempo by time division
		div.s $f0, $f0, $f2	#divide by 1000 to convert microseconds to milliseconds
		swc1 $f0, millisPerDelta #save new milliseconds per delta
		lw $t0, nextOffset	#load calculated next offset
		sw $t0, eventOffset	#set to next offset
		j playNextEvent
			noTempoFound:
			#silence ongoing tones
			li $v0, 62
			syscall
			lw $t0, nextOffset
			sw $t0, eventOffset
			j playNextEvent
		
		
###################################
#exit: gracefully exits the program
###################################
exit: 

li $v0, 10
syscall
############################################################################################
# DATA SEGMENT										   #
#	save vars:									   #
# 		$s0 stores file descriptor						   #
# 		$s1 stores current paramater halfword					   #
# 		$s2 stores current event word to be written to playback queue		   #
#		$s3 stores current status byte						   #
#		$s4 stores current note offset						   #
############################################################################################
	.data
_fileName: .asciiz "Please specifiy a midi file:"
_fileNameLength: .asciiz "Filename too long!"
_loadTrackError: .asciiz "Could not load file!"
_midiTypeError: .asciiz "Track type not supported!"
_playAnother: .asciiz "Would you like to play another file?"
_readingFile: .asciiz "Reading file and loading events into memory.\n"
_playingFile: .asciiz "Playing loaded track.\n"
_skip: .asciiz "Skipping ahead 10%.\n"
_rewind: .asciiz "Rewinding back 10%.\n"
_done: .asciiz "Done.\n"
_paused: .asciiz "Pause.\n"
_resumed: .asciiz "Resume.\n"
_newLine: .asciiz "\n"


fileDescriptor: .space 4	#Descriptor for current file
buffer: .byte 0			#Buffer for bytes read from file
lastStatus:  .align 2
		.byte 0		#last status byte received
statusFlag: .word 0		#Positive if current status is support, negative if it is not
isStatus: .word 0		#Positive if current byte is a status byte, negative if not
format: .space 2 		#MIDI format
tracks: .space 2		#Number of track
timeDiv: .word 0		#Time division, default 50000
delta: .word 4			#Current Event delta time
tempo: .word 500000		#Tempo, default 120
millisPerDelta: .float 0.00	#milliseconds per Delta Tick
trackEnd: .word 0xFF2F00	#Track end signature
fileName: .space 120		#File name buffer
eventOffset: .word 0		#Note write offset
endOfTrackOffset: .word	0		#offset that points to the last note in the playback queue
recv_data:	.word 0xffff0004# Register which holds received chars
recv_ctrl:	.word 0xffff0000# Register which indicates when data is available
paused:	.word  -1		#indicates if the player is paused
skipSize: .word  0		#10 percent of track length
nextOffset: .word 0		#stores the value of where the note offset will be set after skip or reverse
notes: 	.align 2
	.space 52428800		#Stored notes ready to play (reserve 50MB)
