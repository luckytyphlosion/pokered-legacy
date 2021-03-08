INCLUDE "constants.asm"


SECTION "NULL", ROM0
NULL::

INCLUDE "home/header.asm"


SECTION "High Home", ROM0

INCLUDE "home/lcd.asm"
INCLUDE "home/clear_sprites.asm"
INCLUDE "home/copy.asm"


SECTION "Home", ROM0

INCLUDE "home/start.asm"
INCLUDE "home/joypad.asm"

INCLUDE "data/maps/map_header_pointers.asm"

INCLUDE "home/overworld.asm"

DrawHPBar::
; Draw an HP bar d tiles long, and fill it to e pixels.
; If c is nonzero, show at least a sliver regardless.
; The right end of the bar changes with [wHPBarType].

	push hl
	push de
	push bc

	; Left
	ld a, $71 ; "HP:"
	ld [hli], a
	ld a, $62
	ld [hli], a

	push hl

	; Middle
	ld a, $63 ; empty
.draw
	ld [hli], a
	dec d
	jr nz, .draw

	; Right
	ld a, [wHPBarType]
	dec a
	ld a, $6d ; status screen and battle
	jr z, .ok
	dec a ; pokemon menu
.ok
	ld [hl], a

	pop hl

	ld a, e
	and a
	jr nz, .fill

	; If c is nonzero, draw a pixel anyway.
	ld a, c
	and a
	jr z, .done
	ld e, 1

.fill
	ld a, e
	sub 8
	jr c, .partial
	ld e, a
	ld a, $6b ; full
	ld [hli], a
	ld a, e
	and a
	jr z, .done
	jr .fill

.partial
	; Fill remaining pixels at the end if necessary.
	ld a, $63 ; empty
	add e
	ld [hl], a
.done
	pop bc
	pop de
	pop hl
	ret

; loads pokemon data from one of multiple sources to wLoadedMon
; loads base stats to wMonHeader
; INPUT:
; [wWhichPokemon] = index of pokemon within party/box
; [wMonDataLocation] = source
; 00: player's party
; 01: enemy's party
; 02: current box
; 03: daycare
; OUTPUT:
; [wcf91] = pokemon ID
; wLoadedMon = base address of pokemon data
; wMonHeader = base address of base stats
LoadMonData::
	jpfar LoadMonData_

OverwritewMoves::
; Write c to [wMoves + b]. Unused.
	ld hl, wMoves
	ld e, b
	ld d, 0
	add hl, de
	ld a, c
	ld [hl], a
	ret

LoadFlippedFrontSpriteByMonIndex::
	ld a, 1
	ld [wSpriteFlipped], a

LoadFrontSpriteByMonIndex::
	push hl
	ld a, [wd11e]
	push af
	ld a, [wcf91]
	ld [wd11e], a
	predef IndexToPokedex
	ld hl, wd11e
	ld a, [hl]
	pop bc
	ld [hl], b
	and a
	pop hl
	jr z, .invalidDexNumber ; dex #0 invalid
	cp NUM_POKEMON + 1
	jr c, .validDexNumber   ; dex >#151 invalid
.invalidDexNumber
	ld a, RHYDON ; $1
	ld [wcf91], a
	ret
.validDexNumber
	push hl
	ld de, vFrontPic
	call LoadMonFrontSprite
	pop hl
	ldh a, [hLoadedROMBank]
	push af
	ld a, BANK(CopyUncompressedPicToHL)
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	xor a
	ldh [hStartTileID], a
	call CopyUncompressedPicToHL
	xor a
	ld [wSpriteFlipped], a
	pop af
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	ret


PlayCry::
; Play monster a's cry.
	call GetCryData
	call PlaySound
	jp WaitForSoundToFinish

GetCryData::
; Load cry data for monster a.
	dec a
	ld c, a
	ld b, 0
	ld hl, CryData
	add hl, bc
	add hl, bc
	add hl, bc

	ld a, BANK(CryData)
	call BankswitchHome
	ld a, [hli]
	ld b, a ; cry id
	ld a, [hli]
	ld [wFrequencyModifier], a
	ld a, [hl]
	ld [wTempoModifier], a
	call BankswitchBack

	; Cry headers have 3 channels,
	; and start from index CRY_SFX_START,
	; so add 3 times the cry id.
	ld a, b
	ld c, CRY_SFX_START
	rlca ; * 2
	add b
	add c
	ret

DisplayPartyMenu::
	ldh a, [hTileAnimations]
	push af
	xor a
	ldh [hTileAnimations], a
	call GBPalWhiteOutWithDelay3
	call ClearSprites
	call PartyMenuInit
	call DrawPartyMenu
	jp HandlePartyMenuInput

GoBackToPartyMenu::
	ldh a, [hTileAnimations]
	push af
	xor a
	ldh [hTileAnimations], a
	call PartyMenuInit
	call RedrawPartyMenu
	jp HandlePartyMenuInput

PartyMenuInit::
	ld a, 1 ; hardcoded bank
	call BankswitchHome
	call LoadHpBarAndStatusTilePatterns
	ld hl, wd730
	set 6, [hl] ; turn off letter printing delay
	xor a ; PLAYER_PARTY_DATA
	ld [wMonDataLocation], a
	ld [wMenuWatchMovingOutOfBounds], a
	ld hl, wTopMenuItemY
	inc a
	ld [hli], a ; top menu item Y
	xor a
	ld [hli], a ; top menu item X
	ld a, [wPartyAndBillsPCSavedMenuItem]
	push af
	ld [hli], a ; current menu item ID
	inc hl
	ld a, [wPartyCount]
	and a ; are there more than 0 pokemon in the party?
	jr z, .storeMaxMenuItemID
	dec a
; if party is not empty, the max menu item ID is ([wPartyCount] - 1)
; otherwise, it is 0
.storeMaxMenuItemID
	ld [hli], a ; max menu item ID
	ld a, [wForcePlayerToChooseMon]
	and a
	ld a, A_BUTTON | B_BUTTON
	jr z, .next
	xor a
	ld [wForcePlayerToChooseMon], a
	inc a ; a = A_BUTTON
.next
	ld [hli], a ; menu watched keys
	pop af
	ld [hl], a ; old menu item ID
	ret

HandlePartyMenuInput::
	ld a, 1
	ld [wMenuWrappingEnabled], a
	ld a, $40
	ld [wPartyMenuAnimMonEnabled], a
	call HandleMenuInput_
	call PlaceUnfilledArrowMenuCursor
	ld b, a
	xor a
	ld [wPartyMenuAnimMonEnabled], a
	ld a, [wCurrentMenuItem]
	ld [wPartyAndBillsPCSavedMenuItem], a
	ld hl, wd730
	res 6, [hl] ; turn on letter printing delay
	ld a, [wMenuItemToSwap]
	and a
	jp nz, .swappingPokemon
	pop af
	ldh [hTileAnimations], a
	bit 1, b
	jr nz, .noPokemonChosen
	ld a, [wPartyCount]
	and a
	jr z, .noPokemonChosen
	ld a, [wCurrentMenuItem]
	ld [wWhichPokemon], a
	ld hl, wPartySpecies
	ld b, 0
	ld c, a
	add hl, bc
	ld a, [hl]
	ld [wcf91], a
	ld [wBattleMonSpecies2], a
	call BankswitchBack
	and a
	ret
.noPokemonChosen
	call BankswitchBack
	scf
	ret
.swappingPokemon
	bit 1, b ; was the B button pressed?
	jr z, .handleSwap ; if not, handle swapping the pokemon
.cancelSwap ; if the B button was pressed
	farcall ErasePartyMenuCursors
	xor a
	ld [wMenuItemToSwap], a
	ld [wPartyMenuTypeOrMessageID], a
	call RedrawPartyMenu
	jr HandlePartyMenuInput
.handleSwap
	ld a, [wCurrentMenuItem]
	ld [wWhichPokemon], a
	farcall SwitchPartyMon
	jr HandlePartyMenuInput

DrawPartyMenu::
	ld hl, DrawPartyMenu_
	jr DrawPartyMenuCommon

RedrawPartyMenu::
	ld hl, RedrawPartyMenu_

DrawPartyMenuCommon::
	ld b, BANK(RedrawPartyMenu_)
	jp Bankswitch

; prints a pokemon's status condition
; INPUT:
; de = address of status condition
; hl = destination address
PrintStatusCondition::
	push de
	dec de
	dec de ; de = address of current HP
	ld a, [de]
	ld b, a
	dec de
	ld a, [de]
	or b ; is the pokemon's HP zero?
	pop de
	jr nz, PrintStatusConditionNotFainted
; if the pokemon's HP is 0, print "FNT"
	ld a, "F"
	ld [hli], a
	ld a, "N"
	ld [hli], a
	ld [hl], "T"
	and a
	ret

PrintStatusConditionNotFainted::
	homecall_sf PrintStatusAilment
	ret

; function to print pokemon level, leaving off the ":L" if the level is at least 100
; INPUT:
; hl = destination address
; [wLoadedMonLevel] = level
PrintLevel::
	ld a, "<LV>" ; ":L" tile ID
	ld [hli], a
	ld c, 2 ; number of digits
	ld a, [wLoadedMonLevel] ; level
	cp 100
	jr c, PrintLevelCommon
; if level at least 100, write over the ":L" tile
	dec hl
	inc c ; increment number of digits to 3
	jr PrintLevelCommon

; prints the level without leaving off ":L" regardless of level
; INPUT:
; hl = destination address
; [wLoadedMonLevel] = level
PrintLevelFull::
	ld a, "<LV>" ; ":L" tile ID
	ld [hli], a
	ld c, 3 ; number of digits
	ld a, [wLoadedMonLevel] ; level

PrintLevelCommon::
	ld [wd11e], a
	ld de, wd11e
	ld b, LEFT_ALIGN | 1 ; 1 byte
	jp PrintNumber

GetwMoves::
; Unused. Returns the move at index a from wMoves in a
	ld hl, wMoves
	ld c, a
	ld b, 0
	add hl, bc
	ld a, [hl]
	ret

; copies the base stat data of a pokemon to wMonHeader
; INPUT:
; [wd0b5] = pokemon ID
GetMonHeader::
	ldh a, [hLoadedROMBank]
	push af
	ld a, BANK(BaseStats)
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	push bc
	push de
	push hl
	ld a, [wd11e]
	push af
	ld a, [wd0b5]
	ld [wd11e], a
	ld de, FossilKabutopsPic
	ld b, $66 ; size of Kabutops fossil and Ghost sprites
	cp FOSSIL_KABUTOPS ; Kabutops fossil
	jr z, .specialID
	ld de, GhostPic
	cp MON_GHOST ; Ghost
	jr z, .specialID
	ld de, FossilAerodactylPic
	ld b, $77 ; size of Aerodactyl fossil sprite
	cp FOSSIL_AERODACTYL ; Aerodactyl fossil
	jr z, .specialID
	cp MEW
	jr z, .mew
	predef IndexToPokedex   ; convert pokemon ID in [wd11e] to pokedex number
	ld a, [wd11e]
	dec a
	ld bc, MonBaseStatsEnd - MonBaseStats
	ld hl, BaseStats
	call AddNTimes
	ld de, wMonHeader
	ld bc, MonBaseStatsEnd - MonBaseStats
	call CopyData
	jr .done
.specialID
	ld hl, wMonHSpriteDim
	ld [hl], b ; write sprite dimensions
	inc hl
	ld [hl], e ; write front sprite pointer
	inc hl
	ld [hl], d
	jr .done
.mew
	ld hl, MewBaseStats
	ld de, wMonHeader
	ld bc, MonBaseStatsEnd - MonBaseStats
	ld a, BANK(MewBaseStats)
	call FarCopyData
.done
	ld a, [wd0b5]
	ld [wMonHIndex], a
	pop af
	ld [wd11e], a
	pop hl
	pop de
	pop bc
	pop af
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	ret

; copy party pokemon's name to wcd6d
GetPartyMonName2::
	ld a, [wWhichPokemon] ; index within party
	ld hl, wPartyMonNicks

; this is called more often
GetPartyMonName::
	push hl
	push bc
	call SkipFixedLengthTextEntries ; add NAME_LENGTH to hl, a times
	ld de, wcd6d
	push de
	ld bc, NAME_LENGTH
	call CopyData
	pop de
	pop bc
	pop hl
	ret

; function to print a BCD (Binary-coded decimal) number
; de = address of BCD number
; hl = destination address
; c = flags and length
; bit 7: if set, do not print leading zeroes
;        if unset, print leading zeroes
; bit 6: if set, left-align the string (do not pad empty digits with spaces)
;        if unset, right-align the string
; bit 5: if set, print currency symbol at the beginning of the string
;        if unset, do not print the currency symbol
; bits 0-4: length of BCD number in bytes
; Note that bits 5 and 7 are modified during execution. The above reflects
; their meaning at the beginning of the functions's execution.
PrintBCDNumber::
	ld b, c ; save flags in b
	res 7, c
	res 6, c
	res 5, c ; c now holds the length
	bit 5, b
	jr z, .loop
	bit 7, b
	jr nz, .loop
	ld [hl], "¥"
	inc hl
.loop
	ld a, [de]
	swap a
	call PrintBCDDigit ; print upper digit
	ld a, [de]
	call PrintBCDDigit ; print lower digit
	inc de
	dec c
	jr nz, .loop
	bit 7, b ; were any non-zero digits printed?
	jr z, .done ; if so, we are done
.numberEqualsZero ; if every digit of the BCD number is zero
	bit 6, b ; left or right alignment?
	jr nz, .skipRightAlignmentAdjustment
	dec hl ; if the string is right-aligned, it needs to be moved back one space
.skipRightAlignmentAdjustment
	bit 5, b
	jr z, .skipCurrencySymbol
	ld [hl], "¥"
	inc hl
.skipCurrencySymbol
	ld [hl], "0"
	call PrintLetterDelay
	inc hl
.done
	ret

PrintBCDDigit::
	and $f
	and a
	jr z, .zeroDigit
.nonzeroDigit
	bit 7, b ; have any non-space characters been printed?
	jr z, .outputDigit
; if bit 7 is set, then no numbers have been printed yet
	bit 5, b ; print the currency symbol?
	jr z, .skipCurrencySymbol
	ld [hl], "¥"
	inc hl
	res 5, b
.skipCurrencySymbol
	res 7, b ; unset 7 to indicate that a nonzero digit has been reached
.outputDigit
	add "0"
	ld [hli], a
	jp PrintLetterDelay
.zeroDigit
	bit 7, b ; either printing leading zeroes or already reached a nonzero digit?
	jr z, .outputDigit ; if so, print a zero digit
	bit 6, b ; left or right alignment?
	ret nz
	inc hl ; if right-aligned, "print" a space by advancing the pointer
	ret

; uncompresses the front or back sprite of the specified mon
; assumes the corresponding mon header is already loaded
; hl contains offset to sprite pointer ($b for front or $d for back)
UncompressMonSprite::
	ld bc, wMonHeader
	add hl, bc
	ld a, [hli]
	ld [wSpriteInputPtr], a    ; fetch sprite input pointer
	ld a, [hl]
	ld [wSpriteInputPtr+1], a
; define (by index number) the bank that a pokemon's image is in
; index = Mew, bank 1
; index = Kabutops fossil, bank $B
; index < $1F, bank 9
; $1F ≤ index < $4A, bank $A
; $4A ≤ index < $74, bank $B
; $74 ≤ index < $99, bank $C
; $99 ≤ index,       bank $D
	ld a, [wcf91] ; XXX name for this ram location
	ld b, a
	cp MEW
	ld a, BANK(MewPicFront)
	jr z, .GotBank
	ld a, b
	cp FOSSIL_KABUTOPS
	ld a, BANK(FossilKabutopsPic)
	jr z, .GotBank
	ld a, b
	cp TANGELA + 1
	ld a, BANK(TangelaPicFront)
	jr c, .GotBank
	ld a, b
	cp MOLTRES + 1
	ld a, BANK(MoltresPicFront)
	jr c, .GotBank
	ld a, b
	cp BEEDRILL + 2
	ld a, BANK(BeedrillPicFront)
	jr c, .GotBank
	ld a, b
	cp STARMIE + 1
	ld a, BANK(StarmiePicFront)
	jr c, .GotBank
	ld a, BANK(VictreebelPicFront)
.GotBank
	jp UncompressSpriteData

; de: destination location
LoadMonFrontSprite::
	push de
	ld hl, wMonHFrontSprite - wMonHeader
	call UncompressMonSprite
	ld hl, wMonHSpriteDim
	ld a, [hli]
	ld c, a
	pop de
	; fall through

; postprocesses uncompressed sprite chunks to a 2bpp sprite and loads it into video ram
; calculates alignment parameters to place both sprite chunks in the center of the 7*7 tile sprite buffers
; de: destination location
; a,c:  sprite dimensions (in tiles of 8x8 each)
LoadUncompressedSpriteData::
	push de
	and $f
	ldh [hSpriteWidth], a ; each byte contains 8 pixels (in 1bpp), so tiles=bytes for width
	ld b, a
	ld a, $7
	sub b      ; 7-w
	inc a      ; 8-w
	srl a      ; (8-w)/2     ; horizontal center (in tiles, rounded up)
	ld b, a
	add a
	add a
	add a
	sub b      ; 7*((8-w)/2) ; skip for horizontal center (in tiles)
	ldh [hSpriteOffset], a
	ld a, c
	swap a
	and $f
	ld b, a
	add a
	add a
	add a     ; 8*tiles is height in bytes
	ldh [hSpriteHeight], a
	ld a, $7
	sub b      ; 7-h         ; skip for vertical center (in tiles, relative to current column)
	ld b, a
	ldh a, [hSpriteOffset]
	add b     ; 7*((8-w)/2) + 7-h ; combined overall offset (in tiles)
	add a
	add a
	add a     ; 8*(7*((8-w)/2) + 7-h) ; combined overall offset (in bytes)
	ldh [hSpriteOffset], a
	xor a
	ld [MBC1SRamBank], a
	ld hl, sSpriteBuffer0
	call ZeroSpriteBuffer   ; zero buffer 0
	ld de, sSpriteBuffer1
	ld hl, sSpriteBuffer0
	call AlignSpriteDataCentered    ; copy and align buffer 1 to 0 (containing the MSB of the 2bpp sprite)
	ld hl, sSpriteBuffer1
	call ZeroSpriteBuffer   ; zero buffer 1
	ld de, sSpriteBuffer2
	ld hl, sSpriteBuffer1
	call AlignSpriteDataCentered    ; copy and align buffer 2 to 1 (containing the LSB of the 2bpp sprite)
	pop de
	jp InterlaceMergeSpriteBuffers

; copies and aligns the sprite data properly inside the sprite buffer
; sprite buffers are 7*7 tiles in size, the loaded sprite is centered within this area
AlignSpriteDataCentered::
	ldh a, [hSpriteOffset]
	ld b, $0
	ld c, a
	add hl, bc
	ldh a, [hSpriteWidth]
.columnLoop
	push af
	push hl
	ldh a, [hSpriteHeight]
	ld c, a
.columnInnerLoop
	ld a, [de]
	inc de
	ld [hli], a
	dec c
	jr nz, .columnInnerLoop
	pop hl
	ld bc, 7*8    ; 7 tiles
	add hl, bc    ; advance one full column
	pop af
	dec a
	jr nz, .columnLoop
	ret

; fills the sprite buffer (pointed to in hl) with zeros
ZeroSpriteBuffer::
	ld bc, SPRITEBUFFERSIZE
.nextByteLoop
	xor a
	ld [hli], a
	dec bc
	ld a, b
	or c
	jr nz, .nextByteLoop
	ret

; combines the (7*7 tiles, 1bpp) sprite chunks in buffer 0 and 1 into a 2bpp sprite located in buffer 1 through 2
; in the resulting sprite, the rows of the two source sprites are interlaced
; de: output address
InterlaceMergeSpriteBuffers::
	xor a
	ld [MBC1SRamBank], a
	push de
	ld hl, sSpriteBuffer2 + (SPRITEBUFFERSIZE - 1) ; destination: end of buffer 2
	ld de, sSpriteBuffer1 + (SPRITEBUFFERSIZE - 1) ; source 2: end of buffer 1
	ld bc, sSpriteBuffer0 + (SPRITEBUFFERSIZE - 1) ; source 1: end of buffer 0
	ld a, SPRITEBUFFERSIZE/2 ; $c4
	ldh [hSpriteInterlaceCounter], a
.interlaceLoop
	ld a, [de]
	dec de
	ld [hld], a   ; write byte of source 2
	ld a, [bc]
	dec bc
	ld [hld], a   ; write byte of source 1
	ld a, [de]
	dec de
	ld [hld], a   ; write byte of source 2
	ld a, [bc]
	dec bc
	ld [hld], a   ; write byte of source 1
	ldh a, [hSpriteInterlaceCounter]
	dec a
	ldh [hSpriteInterlaceCounter], a
	jr nz, .interlaceLoop
	ld a, [wSpriteFlipped]
	and a
	jr z, .notFlipped
	ld bc, 2*SPRITEBUFFERSIZE
	ld hl, sSpriteBuffer1
.swapLoop
	swap [hl]    ; if flipped swap nybbles in all bytes
	inc hl
	dec bc
	ld a, b
	or c
	jr nz, .swapLoop
.notFlipped
	pop hl
	ld de, sSpriteBuffer1
	ld c, (2*SPRITEBUFFERSIZE)/16 ; $31, number of 16 byte chunks to be copied
	ldh a, [hLoadedROMBank]
	ld b, a
	jp CopyVideoData

INCLUDE "data/tilesets/collision_tile_ids.asm"

INCLUDE "home/copy2.asm"
INCLUDE "home/text.asm"
INCLUDE "home/vcopy.asm"
INCLUDE "home/init.asm"
INCLUDE "home/vblank.asm"
INCLUDE "home/fade.asm"
INCLUDE "home/serial.asm"
INCLUDE "home/timer.asm"
INCLUDE "home/audio.asm"

UpdateSprites::
	ld a, [wUpdateSpritesEnabled]
	dec a
	ret nz
	homecall _UpdateSprites
	ret


INCLUDE "data/items/marts.asm"

INCLUDE "home/overworld_text.asm"
INCLUDE "home/uncompress.asm"

ResetPlayerSpriteData::
	ld hl, wSpriteStateData1
	call ResetPlayerSpriteData_ClearSpriteData
	ld hl, wSpriteStateData2
	call ResetPlayerSpriteData_ClearSpriteData
	ld a, $1
	ld [wSpritePlayerStateData1PictureID], a
	ld [wSpritePlayerStateData2ImageBaseOffset], a
	ld hl, wSpritePlayerStateData1YPixels
	ld [hl], $3c     ; set Y screen pos
	inc hl
	inc hl
	ld [hl], $40     ; set X screen pos
	ret

; overwrites sprite data with zeroes
ResetPlayerSpriteData_ClearSpriteData::
	ld bc, $10
	xor a
	jp FillMemory

FadeOutAudio::
	ld a, [wAudioFadeOutControl]
	and a ; currently fading out audio?
	jr nz, .fadingOut
	ld a, [wd72c]
	bit 1, a
	ret nz
	ld a, $77
	ldh [rNR50], a
	ret
.fadingOut
	ld a, [wAudioFadeOutCounter]
	and a
	jr z, .counterReachedZero
	dec a
	ld [wAudioFadeOutCounter], a
	ret
.counterReachedZero
	ld a, [wAudioFadeOutCounterReloadValue]
	ld [wAudioFadeOutCounter], a
	ldh a, [rNR50]
	and a ; has the volume reached 0?
	jr z, .fadeOutComplete
	ld b, a
	and $f
	dec a
	ld c, a
	ld a, b
	and $f0
	swap a
	dec a
	swap a
	or c
	ldh [rNR50], a
	ret
.fadeOutComplete
	ld a, [wAudioFadeOutControl]
	ld b, a
	xor a
	ld [wAudioFadeOutControl], a
	ld a, SFX_STOP_ALL_MUSIC
	ld [wNewSoundID], a
	call PlaySound
	ld a, [wAudioSavedROMBank]
	ld [wAudioROMBank], a
	ld a, b
	ld [wNewSoundID], a
	jp PlaySound

INCLUDE "home/text_script.asm"
INCLUDE "home/start_menu.asm"

; function to count how many bits are set in a string of bytes
; INPUT:
; hl = address of string of bytes
; b = length of string of bytes
; OUTPUT:
; [wNumSetBits] = number of set bits
CountSetBits::
	ld c, 0
.loop
	ld a, [hli]
	ld e, a
	ld d, 8
.innerLoop ; count how many bits are set in the current byte
	srl e
	ld a, 0
	adc c
	ld c, a
	dec d
	jr nz, .innerLoop
	dec b
	jr nz, .loop
	ld a, c
	ld [wNumSetBits], a
	ret

; subtracts the amount the player paid from their money
; OUTPUT: carry = 0(success) or 1(fail because there is not enough money)
SubtractAmountPaidFromMoney::
	farjp SubtractAmountPaidFromMoney_

; adds the amount the player sold to their money
AddAmountSoldToMoney::
	ld de, wPlayerMoney + 2
	ld hl, hMoney + 2 ; total price of items
	ld c, 3 ; length of money in bytes
	predef AddBCDPredef ; add total price to money
	ld a, MONEY_BOX
	ld [wTextBoxID], a
	call DisplayTextBoxID ; redraw money text box
	ld a, SFX_PURCHASE
	call PlaySoundWaitForCurrent
	jp WaitForSoundToFinish

; function to remove an item (in varying quantities) from the player's bag or PC box
; INPUT:
; HL = address of inventory (either wNumBagItems or wNumBoxItems)
; [wWhichPokemon] = index (within the inventory) of the item to remove
; [wItemQuantity] = quantity to remove
RemoveItemFromInventory::
	homecall RemoveItemFromInventory_
	ret

; function to add an item (in varying quantities) to the player's bag or PC box
; INPUT:
; HL = address of inventory (either wNumBagItems or wNumBoxItems)
; [wcf91] = item ID
; [wItemQuantity] = item quantity
; sets carry flag if successful, unsets carry flag if unsuccessful
AddItemToInventory::
	push bc
	homecall_sf AddItemToInventory_
	pop bc
	ret

INCLUDE "home/list_menu.asm"
INCLUDE "home/names.asm"

; reloads text box tile patterns, current map view, and tileset tile patterns
ReloadMapData::
	ldh a, [hLoadedROMBank]
	push af
	ld a, [wCurMap]
	call SwitchToMapRomBank
	call DisableLCD
	call LoadTextBoxTilePatterns
	call LoadCurrentMapView
	call LoadTilesetTilePatternData
	call EnableLCD
	pop af
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	ret

; reloads tileset tile patterns
ReloadTilesetTilePatterns::
	ldh a, [hLoadedROMBank]
	push af
	ld a, [wCurMap]
	call SwitchToMapRomBank
	call DisableLCD
	call LoadTilesetTilePatternData
	call EnableLCD
	pop af
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	ret

; shows the town map and lets the player choose a destination to fly to
ChooseFlyDestination::
	ld hl, wd72e
	res 4, [hl]
	farjp LoadTownMap_Fly

; causes the text box to close without waiting for a button press after displaying text
DisableWaitingAfterTextDisplay::
	ld a, $01
	ld [wDoNotWaitForButtonPressAfterDisplayingText], a
	ret

; uses an item
; UseItem is used with dummy items to perform certain other functions as well
; INPUT:
; [wcf91] = item ID
; OUTPUT:
; [wActionResultOrTookBattleTurn] = success
; 00: unsuccessful
; 01: successful
; 02: not able to be used right now, no extra menu displayed (only certain items use this)
UseItem::
	farjp UseItem_

; confirms the item toss and then tosses the item
; INPUT:
; hl = address of inventory (either wNumBagItems or wNumBoxItems)
; [wcf91] = item ID
; [wWhichPokemon] = index of item within inventory
; [wItemQuantity] = quantity to toss
; OUTPUT:
; clears carry flag if the item is tossed, sets carry flag if not
TossItem::
	ldh a, [hLoadedROMBank]
	push af
	ld a, BANK(TossItem_)
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	call TossItem_
	pop de
	ld a, d
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	ret

; checks if an item is a key item
; INPUT:
; [wcf91] = item ID
; OUTPUT:
; [wIsKeyItem] = result
; 00: item is not key item
; 01: item is key item
IsKeyItem::
	push hl
	push de
	push bc
	farcall IsKeyItem_
	pop bc
	pop de
	pop hl
	ret

; function to draw various text boxes
; INPUT:
; [wTextBoxID] = text box ID
; b, c = y, x cursor position (TWO_OPTION_MENU only)
DisplayTextBoxID::
	homecall_sf DisplayTextBoxID_
	ret

INCLUDE "home/npc_movement.asm"

DebugPressedOrHeldB::
IF DEF(_DEBUG)
	ld a, [wd732]
	bit 1, a
	ret z
	ldh a, [hJoyHeld]
	bit BIT_B_BUTTON, a
	ret nz
	ldh a, [hJoyPressed]
	bit BIT_B_BUTTON, a
ENDC
	ret

INCLUDE "home/trainers.asm"

; checks if the player's coordinates match an arrow movement tile's coordinates
; and if so, decodes the RLE movement data
; b = player Y
; c = player X
DecodeArrowMovementRLE::
	ld a, [hli]
	cp $ff
	ret z ; no match in the list
	cp b
	jr nz, .nextArrowMovementTileEntry1
	ld a, [hli]
	cp c
	jr nz, .nextArrowMovementTileEntry2
	ld a, [hli]
	ld d, [hl]
	ld e, a
	ld hl, wSimulatedJoypadStatesEnd
	call DecodeRLEList
	dec a
	ld [wSimulatedJoypadStatesIndex], a
	ret
.nextArrowMovementTileEntry1
	inc hl
.nextArrowMovementTileEntry2
	inc hl
	inc hl
	jr DecodeArrowMovementRLE

TextScript_ItemStoragePC::
	call SaveScreenTilesToBuffer2
	ld b, BANK(PlayerPC)
	ld hl, PlayerPC
	jr bankswitchAndContinue

TextScript_BillsPC::
	call SaveScreenTilesToBuffer2
	ld b, BANK(BillsPC_)
	ld hl, BillsPC_
	jr bankswitchAndContinue

TextScript_GameCornerPrizeMenu::
; XXX find a better name for this function
; special_F7
	ld b, BANK(CeladonPrizeMenu)
	ld hl, CeladonPrizeMenu
bankswitchAndContinue::
	call Bankswitch
	jp HoldTextDisplayOpen        ; continue to main text-engine function

TextScript_PokemonCenterPC::
	ld b, BANK(ActivatePC)
	ld hl, ActivatePC
	jr bankswitchAndContinue

StartSimulatingJoypadStates::
	xor a
	ld [wOverrideSimulatedJoypadStatesMask], a
	ld [wSpritePlayerStateData2MovementByte1], a
	ld hl, wd730
	set 7, [hl]
	ret

IsItemInBag::
; given an item_id in b
; set zero flag if item isn't in player's bag
; else reset zero flag
; related to Pokémon Tower and ghosts
	predef GetQuantityOfItemInBag
	ld a, b
	and a
	ret

DisplayPokedex::
	ld [wd11e], a
	farjp _DisplayPokedex

SetSpriteFacingDirectionAndDelay::
	call SetSpriteFacingDirection
	ld c, 6
	jp DelayFrames

SetSpriteFacingDirection::
	ld a, SPRITESTATEDATA1_FACINGDIRECTION
	ldh [hSpriteDataOffset], a
	call GetPointerWithinSpriteStateData1
	ldh a, [hSpriteFacingDirection]
	ld [hl], a
	ret

SetSpriteImageIndexAfterSettingFacingDirection::
	ld de, SPRITESTATEDATA1_IMAGEINDEX - SPRITESTATEDATA1_FACINGDIRECTION
	add hl, de
	ld [hl], a
	ret

; tests if the player's coordinates are in a specified array
; INPUT:
; hl = address of array
; OUTPUT:
; [wCoordIndex] = if there is match, the matching array index
; sets carry if the coordinates are in the array, clears carry if not
ArePlayerCoordsInArray::
	ld a, [wYCoord]
	ld b, a
	ld a, [wXCoord]
	ld c, a
	; fallthrough

CheckCoords::
	xor a
	ld [wCoordIndex], a
.loop
	ld a, [hli]
	cp $ff ; reached terminator?
	jr z, .notInArray
	push hl
	ld hl, wCoordIndex
	inc [hl]
	pop hl
.compareYCoord
	cp b
	jr z, .compareXCoord
	inc hl
	jr .loop
.compareXCoord
	ld a, [hli]
	cp c
	jr nz, .loop
.inArray
	scf
	ret
.notInArray
	and a
	ret

; tests if a boulder's coordinates are in a specified array
; INPUT:
; hl = address of array
; [hSpriteIndex] = index of boulder sprite
; OUTPUT:
; [wCoordIndex] = if there is match, the matching array index
; sets carry if the coordinates are in the array, clears carry if not
CheckBoulderCoords::
	push hl
	ld hl, wSpritePlayerStateData2MapY
	ldh a, [hSpriteIndex]
	swap a
	ld d, $0
	ld e, a
	add hl, de
	ld a, [hli]
	sub $4 ; because sprite coordinates are offset by 4
	ld b, a
	ld a, [hl]
	sub $4 ; because sprite coordinates are offset by 4
	ld c, a
	pop hl
	jp CheckCoords

GetPointerWithinSpriteStateData1::
	ld h, HIGH(wSpriteStateData1)
	jr _GetPointerWithinSpriteStateData

GetPointerWithinSpriteStateData2::
	ld h, HIGH(wSpriteStateData2)

_GetPointerWithinSpriteStateData:
	ldh a, [hSpriteDataOffset]
	ld b, a
	ldh a, [hSpriteIndex]
	swap a
	add b
	ld l, a
	ret

; decodes a $ff-terminated RLEncoded list
; each entry is a pair of bytes <byte value> <repetitions>
; the final $ff will be replicated in the output list and a contains the number of bytes written
; de: input list
; hl: output list
DecodeRLEList::
	xor a
	ld [wRLEByteCount], a     ; count written bytes here
.listLoop
	ld a, [de]
	cp $ff
	jr z, .endOfList
	ldh [hRLEByteValue], a ; store byte value to be written
	inc de
	ld a, [de]
	ld b, $0
	ld c, a                      ; number of bytes to be written
	ld a, [wRLEByteCount]
	add c
	ld [wRLEByteCount], a     ; update total number of written bytes
	ldh a, [hRLEByteValue]
	call FillMemory              ; write a c-times to output
	inc de
	jr .listLoop
.endOfList
	ld a, $ff
	ld [hl], a                   ; write final $ff
	ld a, [wRLEByteCount]
	inc a                        ; include sentinel in counting
	ret

; sets movement byte 1 for sprite [hSpriteIndex] to $FE and byte 2 to [hSpriteMovementByte2]
SetSpriteMovementBytesToFE::
	push hl
	call GetSpriteMovementByte1Pointer
	ld [hl], $fe
	call GetSpriteMovementByte2Pointer
	ldh a, [hSpriteMovementByte2]
	ld [hl], a
	pop hl
	ret

; sets both movement bytes for sprite [hSpriteIndex] to $FF
SetSpriteMovementBytesToFF::
	push hl
	call GetSpriteMovementByte1Pointer
	ld [hl], $FF
	call GetSpriteMovementByte2Pointer
	ld [hl], $FF ; prevent person from walking?
	pop hl
	ret

; returns the sprite movement byte 1 pointer for sprite [hSpriteIndex] in hl
GetSpriteMovementByte1Pointer::
	ld h, $C2
	ldh a, [hSpriteIndex]
	swap a
	add 6
	ld l, a
	ret

; returns the sprite movement byte 2 pointer for sprite [hSpriteIndex] in hl
GetSpriteMovementByte2Pointer::
	push de
	ld hl, wMapSpriteData
	ldh a, [hSpriteIndex]
	dec a
	add a
	ld d, 0
	ld e, a
	add hl, de
	pop de
	ret

GetTrainerInformation::
	call GetTrainerName
	ld a, [wLinkState]
	and a
	jr nz, .linkBattle
	ld a, BANK(TrainerPicAndMoneyPointers)
	call BankswitchHome
	ld a, [wTrainerClass]
	dec a
	ld hl, TrainerPicAndMoneyPointers
	ld bc, $5
	call AddNTimes
	ld de, wTrainerPicPointer
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hli]
	ld [de], a
	ld de, wTrainerBaseMoney
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hli]
	ld [de], a
	jp BankswitchBack
.linkBattle
	ld hl, wTrainerPicPointer
	ld de, RedPicFront
	ld [hl], e
	inc hl
	ld [hl], d
	ret

GetTrainerName::
	farjp GetTrainerName_

HasEnoughMoney::
; Check if the player has at least as much
; money as the 3-byte BCD value at hMoney.
	ld de, wPlayerMoney
	ld hl, hMoney
	ld c, 3
	jp StringCmp

HasEnoughCoins::
; Check if the player has at least as many
; coins as the 2-byte BCD value at hCoins.
	ld de, wPlayerCoins
	ld hl, hCoins
	ld c, 2
	jp StringCmp

INCLUDE "home/bankswitch.asm"
INCLUDE "home/yes_no.asm"

; calculates the difference |a-b|, setting carry flag if a<b
CalcDifference::
	sub b
	ret nc
	cpl
	add $1
	scf
	ret

MoveSprite::
; move the sprite [hSpriteIndex] with the movement pointed to by de
; actually only copies the movement data to wNPCMovementDirections for later
	call SetSpriteMovementBytesToFF
MoveSprite_::
	push hl
	push bc
	call GetSpriteMovementByte1Pointer
	xor a
	ld [hl], a
	ld hl, wNPCMovementDirections
	ld c, 0

.loop
	ld a, [de]
	ld [hli], a
	inc de
	inc c
	cp -1 ; have we reached the end of the movement data?
	jr nz, .loop

	ld a, c
	ld [wNPCNumScriptedSteps], a ; number of steps taken

	pop bc
	ld hl, wd730
	set 0, [hl]
	pop hl
	xor a
	ld [wOverrideSimulatedJoypadStatesMask], a
	ld [wSimulatedJoypadStatesEnd], a
	dec a
	ld [wJoyIgnore], a
	ld [wWastedByteCD3A], a
	ret

; divides [hDividend2] by [hDivisor2] and stores the quotient in [hQuotient2]
DivideBytes::
	push hl
	ld hl, hQuotient2
	xor a
	ld [hld], a
	ld a, [hld]
	and a
	jr z, .done
	ld a, [hli]
.loop
	sub [hl]
	jr c, .done
	inc hl
	inc [hl]
	dec hl
	jr .loop
.done
	pop hl
	ret

LoadFontTilePatterns::
	ldh a, [rLCDC]
	bit 7, a ; is the LCD enabled?
	jr nz, .on
.off
	ld hl, FontGraphics
	ld de, vFont
	ld bc, FontGraphicsEnd - FontGraphics
	ld a, BANK(FontGraphics)
	jp FarCopyDataDouble ; if LCD is off, transfer all at once
.on
	ld de, FontGraphics
	ld hl, vFont
	lb bc, BANK(FontGraphics), (FontGraphicsEnd - FontGraphics) / $8
	jp CopyVideoDataDouble ; if LCD is on, transfer during V-blank

LoadTextBoxTilePatterns::
	ldh a, [rLCDC]
	bit 7, a ; is the LCD enabled?
	jr nz, .on
.off
	ld hl, TextBoxGraphics
	ld de, vChars2 tile $60
	ld bc, TextBoxGraphicsEnd - TextBoxGraphics
	ld a, BANK(TextBoxGraphics)
	jp FarCopyData2 ; if LCD is off, transfer all at once
.on
	ld de, TextBoxGraphics
	ld hl, vChars2 tile $60
	lb bc, BANK(TextBoxGraphics), (TextBoxGraphicsEnd - TextBoxGraphics) / $10
	jp CopyVideoData ; if LCD is on, transfer during V-blank

LoadHpBarAndStatusTilePatterns::
	ldh a, [rLCDC]
	bit 7, a ; is the LCD enabled?
	jr nz, .on
.off
	ld hl, HpBarAndStatusGraphics
	ld de, vChars2 tile $62
	ld bc, HpBarAndStatusGraphicsEnd - HpBarAndStatusGraphics
	ld a, BANK(HpBarAndStatusGraphics)
	jp FarCopyData2 ; if LCD is off, transfer all at once
.on
	ld de, HpBarAndStatusGraphics
	ld hl, vChars2 tile $62
	lb bc, BANK(HpBarAndStatusGraphics), (HpBarAndStatusGraphicsEnd - HpBarAndStatusGraphics) / $10
	jp CopyVideoData ; if LCD is on, transfer during V-blank

FillMemory::
; Fill bc bytes at hl with a.
	push de
	ld d, a
.loop
	ld a, d
	ld [hli], a
	dec bc
	ld a, b
	or c
	jr nz, .loop
	pop de
	ret

UncompressSpriteFromDE::
; Decompress pic at a:de.
	ld hl, wSpriteInputPtr
	ld [hl], e
	inc hl
	ld [hl], d
	jp UncompressSpriteData


SaveScreenTilesToBuffer2::
	hlcoord 0, 0
	ld de, wTileMapBackup2
	ld bc, SCREEN_WIDTH * SCREEN_HEIGHT
	call CopyData
	ret

LoadScreenTilesFromBuffer2::
	call LoadScreenTilesFromBuffer2DisableBGTransfer
	ld a, 1
	ldh [hAutoBGTransferEnabled], a
	ret

; loads screen tiles stored in wTileMapBackup2 but leaves hAutoBGTransferEnabled disabled
LoadScreenTilesFromBuffer2DisableBGTransfer::
	xor a
	ldh [hAutoBGTransferEnabled], a
	ld hl, wTileMapBackup2
	decoord 0, 0
	ld bc, SCREEN_WIDTH * SCREEN_HEIGHT
	call CopyData
	ret

SaveScreenTilesToBuffer1::
	hlcoord 0, 0
	ld de, wTileMapBackup
	ld bc, SCREEN_WIDTH * SCREEN_HEIGHT
	jp CopyData

LoadScreenTilesFromBuffer1::
	xor a
	ldh [hAutoBGTransferEnabled], a
	ld hl, wTileMapBackup
	decoord 0, 0
	ld bc, SCREEN_WIDTH * SCREEN_HEIGHT
	call CopyData
	ld a, 1
	ldh [hAutoBGTransferEnabled], a
	ret


DelayFrames::
; wait c frames
	call DelayFrame
	dec c
	jr nz, DelayFrames
	ret

PlaySoundWaitForCurrent::
	push af
	call WaitForSoundToFinish
	pop af
	jp PlaySound

; Wait for sound to finish playing
WaitForSoundToFinish::
	ld a, [wLowHealthAlarm]
	and $80
	ret nz
	push hl
.waitLoop
	ld hl, wChannelSoundIDs + Ch5
	xor a
	or [hl]
	inc hl
	or [hl]
	inc hl
	inc hl
	or [hl]
	jr nz, .waitLoop
	pop hl
	ret

NamePointers::
; entries correspond to *_NAME constants
	dw MonsterNames
	dw MoveNames
	dw UnusedBadgeNames
	dw ItemNames
	dw wPartyMonOT ; player's OT names list
	dw wEnemyMonOT ; enemy's OT names list
	dw TrainerNames

GetName::
; arguments:
; [wd0b5] = which name
; [wNameListType] = which list
; [wPredefBank] = bank of list
;
; returns pointer to name in de
	ld a, [wd0b5]
	ld [wd11e], a

	; TM names are separate from item names.
	; BUG: This applies to all names instead of just items.
	cp HM01
	jp nc, GetMachineName

	ldh a, [hLoadedROMBank]
	push af
	push hl
	push bc
	push de
	ld a, [wNameListType]    ;List3759_entrySelector
	dec a
	jr nz, .otherEntries
	;1 = MON_NAMES
	call GetMonName
	ld hl, NAME_LENGTH
	add hl, de
	ld e, l
	ld d, h
	jr .gotPtr
.otherEntries
	;2-7 = OTHER ENTRIES
	ld a, [wPredefBank]
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	ld a, [wNameListType]    ;VariousNames' entryID
	dec a
	add a
	ld d, 0
	ld e, a
	jr nc, .skip
	inc d
.skip
	ld hl, NamePointers
	add hl, de
	ld a, [hli]
	ldh [hSwapTemp + 1], a
	ld a, [hl]
	ldh [hSwapTemp], a
	ldh a, [hSwapTemp]
	ld h, a
	ldh a, [hSwapTemp + 1]
	ld l, a
	ld a, [wd0b5]
	ld b, a
	ld c, 0
.nextName
	ld d, h
	ld e, l
.nextChar
	ld a, [hli]
	cp "@"
	jr nz, .nextChar
	inc c           ;entry counter
	ld a, b          ;wanted entry
	cp c
	jr nz, .nextName
	ld h, d
	ld l, e
	ld de, wcd6d
	ld bc, $14
	call CopyData
.gotPtr
	ld a, e
	ld [wUnusedCF8D], a
	ld a, d
	ld [wUnusedCF8D + 1], a
	pop de
	pop bc
	pop hl
	pop af
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	ret

GetItemPrice::
; Stores item's price as BCD at hItemPrice (3 bytes)
; Input: [wcf91] = item id
	ldh a, [hLoadedROMBank]
	push af
	ld a, [wListMenuID]
	cp MOVESLISTMENU
	ld a, BANK(ItemPrices)
	jr nz, .ok
	ld a, $f ; hardcoded Bank
.ok
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	ld hl, wItemPrices
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wcf91] ; a contains item id
	cp HM01
	jr nc, .getTMPrice
	ld bc, $3
.loop
	add hl, bc
	dec a
	jr nz, .loop
	dec hl
	ld a, [hld]
	ldh [hItemPrice + 2], a
	ld a, [hld]
	ldh [hItemPrice + 1], a
	ld a, [hl]
	ldh [hItemPrice], a
	jr .done
.getTMPrice
	ld a, BANK(GetMachinePrice)
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	call GetMachinePrice
.done
	ld de, hItemPrice
	pop af
	ldh [hLoadedROMBank], a
	ld [MBC1RomBank], a
	ret

; copies a string from [de] to [wcf4b]
CopyStringToCF4B::
	ld hl, wcf4b
	; fall through

; copies a string from [de] to [hl]
CopyString::
	ld a, [de]
	inc de
	ld [hli], a
	cp "@"
	jr nz, CopyString
	ret


INCLUDE "home/joypad2.asm"

; function to do multiplication
; all values are big endian
; INPUT
; FF96-FF98 =  multiplicand
; FF99 = multiplier
; OUTPUT
; FF95-FF98 = product
Multiply::
	push hl
	push bc
	callfar _Multiply
	pop bc
	pop hl
	ret

; function to do division
; all values are big endian
; INPUT
; FF95-FF98 = dividend
; FF99 = divisor
; b = number of bytes in the dividend (starting from FF95)
; OUTPUT
; FF95-FF98 = quotient
; FF99 = remainder
Divide::
	push hl
	push de
	push bc
	homecall _Divide
	pop bc
	pop de
	pop hl
	ret

; This function is used to wait a short period after printing a letter to the
; screen unless the player presses the A/B button or the delay is turned off
; through the [wd730] or [wLetterPrintingDelayFlags] flags.
PrintLetterDelay::
	ld a, [wd730]
	bit 6, a
	ret nz
	ld a, [wLetterPrintingDelayFlags]
	bit 1, a
	ret z
	push hl
	push de
	push bc
	ld a, [wLetterPrintingDelayFlags]
	bit 0, a
	jr z, .waitOneFrame
	ld a, [wOptions]
	and $f
	ldh [hFrameCounter], a
	jr .checkButtons
.waitOneFrame
	ld a, 1
	ldh [hFrameCounter], a
.checkButtons
	call Joypad
	ldh a, [hJoyHeld]
.checkAButton
	bit 0, a ; is the A button pressed?
	jr z, .checkBButton
	jr .endWait
.checkBButton
	bit 1, a ; is the B button pressed?
	jr z, .buttonsNotPressed
.endWait
	call DelayFrame
	jr .done
.buttonsNotPressed ; if neither A nor B is pressed
	ldh a, [hFrameCounter]
	and a
	jr nz, .checkButtons
.done
	pop bc
	pop de
	pop hl
	ret

; Copies [hl, bc) to [de, de + bc - hl).
; In other words, the source data is from hl up to but not including bc,
; and the destination is de.
CopyDataUntil::
	ld a, [hli]
	ld [de], a
	inc de
	ld a, h
	cp b
	jr nz, CopyDataUntil
	ld a, l
	cp c
	jr nz, CopyDataUntil
	ret

; Function to remove a pokemon from the party or the current box.
; wWhichPokemon determines the pokemon.
; [wRemoveMonFromBox] == 0 specifies the party.
; [wRemoveMonFromBox] != 0 specifies the current box.
RemovePokemon::
	jpfar _RemovePokemon

AddPartyMon::
	push hl
	push de
	push bc
	farcall _AddPartyMon
	pop bc
	pop de
	pop hl
	ret

INCLUDE "home/calc_stat.asm"

AddEnemyMonToPlayerParty::
	homecall_sf _AddEnemyMonToPlayerParty
	ret

MoveMon::
	homecall_sf _MoveMon
	ret

; skips a text entries, each of size NAME_LENGTH (like trainer name, OT name, rival name, ...)
; hl: base pointer, will be incremented by NAME_LENGTH * a
SkipFixedLengthTextEntries::
	and a
	ret z
	ld bc, NAME_LENGTH
.skipLoop
	add hl, bc
	dec a
	jr nz, .skipLoop
	ret

AddNTimes::
; add bc to hl a times
	and a
	ret z
.loop
	add hl, bc
	dec a
	jr nz, .loop
	ret

; Compare strings, c bytes in length, at de and hl.
; Often used to compare big endian numbers in battle calculations.
StringCmp::
	ld a, [de]
	cp [hl]
	ret nz
	inc de
	inc hl
	dec c
	jr nz, StringCmp
	ret

; INPUT:
; a = oam block index (each block is 4 oam entries)
; b = Y coordinate of upper left corner of sprite
; c = X coordinate of upper left corner of sprite
; de = base address of 4 tile number and attribute pairs
WriteOAMBlock::
	ld h, HIGH(wOAMBuffer)
	swap a ; multiply by 16
	ld l, a
	call .writeOneEntry ; upper left
	push bc
	ld a, 8
	add c
	ld c, a
	call .writeOneEntry ; upper right
	pop bc
	ld a, 8
	add b
	ld b, a
	call .writeOneEntry ; lower left
	ld a, 8
	add c
	ld c, a
	                      ; lower right
.writeOneEntry
	ld [hl], b ; Y coordinate
	inc hl
	ld [hl], c ; X coordinate
	inc hl
	ld a, [de] ; tile number
	inc de
	ld [hli], a
	ld a, [de] ; attribute
	inc de
	ld [hli], a
	ret

INCLUDE "home/menu_input.asm"

; This toggles a blinking down arrow at hl on and off after a delay has passed.
; This is often called even when no blinking is occurring.
; The reason is that most functions that call this initialize hDownArrowBlinkCount1 to 0.
; The effect is that if the tile at hl is initialized with a down arrow,
; this function will toggle that down arrow on and off, but if the tile isn't
; initialized with a down arrow, this function does nothing.
; That allows this to be called without worrying about if a down arrow should
; be blinking.
HandleDownArrowBlinkTiming::
	ld a, [hl]
	ld b, a
	ld a, "▼"
	cp b
	jr nz, .downArrowOff
.downArrowOn
	ldh a, [hDownArrowBlinkCount1]
	dec a
	ldh [hDownArrowBlinkCount1], a
	ret nz
	ldh a, [hDownArrowBlinkCount2]
	dec a
	ldh [hDownArrowBlinkCount2], a
	ret nz
	ld a, " "
	ld [hl], a
	ld a, $ff
	ldh [hDownArrowBlinkCount1], a
	ld a, $06
	ldh [hDownArrowBlinkCount2], a
	ret
.downArrowOff
	ldh a, [hDownArrowBlinkCount1]
	and a
	ret z
	dec a
	ldh [hDownArrowBlinkCount1], a
	ret nz
	dec a
	ldh [hDownArrowBlinkCount1], a
	ldh a, [hDownArrowBlinkCount2]
	dec a
	ldh [hDownArrowBlinkCount2], a
	ret nz
	ld a, $06
	ldh [hDownArrowBlinkCount2], a
	ld a, "▼"
	ld [hl], a
	ret

; The following code either enables or disables the automatic drawing of
; text boxes by DisplayTextID. Both functions cause DisplayTextID to wait
; for a button press after displaying text (unless [wEnteringCableClub] is set).

EnableAutoTextBoxDrawing::
	xor a
	jr AutoTextBoxDrawingCommon

DisableAutoTextBoxDrawing::
	ld a, TRUE

AutoTextBoxDrawingCommon::
	ld [wAutoTextBoxDrawingControl], a
	xor a
	ld [wDoNotWaitForButtonPressAfterDisplayingText], a ; make DisplayTextID wait for button press
	ret

PrintText::
; Print text hl at (1, 14).
	push hl
	ld a, MESSAGE_BOX
	ld [wTextBoxID], a
	call DisplayTextBoxID
	call UpdateSprites
	call Delay3
	pop hl
PrintText_NoCreatingTextBox::
	bccoord 1, 14
	jp TextCommandProcessor


INCLUDE "home/print_num.asm"

CallFunctionInTable::
; Call function a in jumptable hl.
; de is not preserved.
	push hl
	push de
	push bc
	add a
	ld d, 0
	ld e, a
	add hl, de
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld de, .returnAddress
	push de
	jp hl
.returnAddress
	pop bc
	pop de
	pop hl
	ret

IsInArray::
; Search an array at hl for the value in a.
; Entry size is de bytes.
; Return count b and carry if found.
	ld b, 0

IsInRestOfArray::
	ld c, a
.loop
	ld a, [hl]
	cp -1
	jr z, .notfound
	cp c
	jr z, .found
	inc b
	add hl, de
	jr .loop

.notfound
	and a
	ret

.found
	scf
	ret

RestoreScreenTilesAndReloadTilePatterns::
	call ClearSprites
	ld a, $1
	ld [wUpdateSpritesEnabled], a
	call ReloadMapSpriteTilePatterns
	call LoadScreenTilesFromBuffer2
	call LoadTextBoxTilePatterns
	call RunDefaultPaletteCommand
	jr Delay3

GBPalWhiteOutWithDelay3::
	call GBPalWhiteOut

Delay3::
; The bg map is updated each frame in thirds.
; Wait three frames to let the bg map fully update.
	ld c, 3
	jp DelayFrames

GBPalNormal::
; Reset BGP and OBP0.
	ld a, %11100100 ; 3210
	ldh [rBGP], a
	ld a, %11010000 ; 3100
	ldh [rOBP0], a
	ret

GBPalWhiteOut::
; White out all palettes.
	xor a
	ldh [rBGP], a
	ldh [rOBP0], a
	ldh [rOBP1], a
	ret

RunDefaultPaletteCommand::
	ld b, SET_PAL_DEFAULT
RunPaletteCommand::
	ld a, [wOnSGB]
	and a
	ret z
	predef_jump _RunPaletteCommand

GetHealthBarColor::
; Return at hl the palette of
; an HP bar e pixels long.
	ld a, e
	cp 27
	ld d, 0 ; green
	jr nc, .gotColor
	cp 10
	inc d ; yellow
	jr nc, .gotColor
	inc d ; red
.gotColor
	ld [hl], d
	ret

; Copy the current map's sprites' tile patterns to VRAM again after they have
; been overwritten by other tile patterns.
ReloadMapSpriteTilePatterns::
	ld hl, wFontLoaded
	ld a, [hl]
	push af
	res 0, [hl]
	push hl
	xor a
	ld [wSpriteSetID], a
	call DisableLCD
	farcall InitMapSprites
	call EnableLCD
	pop hl
	pop af
	ld [hl], a
	call LoadPlayerSpriteGraphics
	call LoadFontTilePatterns
	jp UpdateSprites

GiveItem::
; Give player quantity c of item b,
; and copy the item's name to wcf4b.
; Return carry on success.
	ld a, b
	ld [wd11e], a
	ld [wcf91], a
	ld a, c
	ld [wItemQuantity], a
	ld hl, wNumBagItems
	call AddItemToInventory
	ret nc
	call GetItemName
	call CopyStringToCF4B
	scf
	ret

GivePokemon::
; Give the player monster b at level c.
	ld a, b
	ld [wcf91], a
	ld a, c
	ld [wCurEnemyLVL], a
	xor a ; PLAYER_PARTY_DATA
	ld [wMonDataLocation], a
	farjp _GivePokemon

Random::
; Return a random number in a.
; For battles, use BattleRandom.
	push hl
	push de
	push bc
	farcall Random_
	ldh a, [hRandomAdd]
	pop bc
	pop de
	pop hl
	ret

INCLUDE "home/predef.asm"

UpdateCinnabarGymGateTileBlocks::
	farjp UpdateCinnabarGymGateTileBlocks_

CheckForHiddenObjectOrBookshelfOrCardKeyDoor::
	ldh a, [hLoadedROMBank]
	push af
	ldh a, [hJoyHeld]
	bit 0, a ; A button
	jr z, .nothingFound
; A button is pressed
	ld a, BANK(CheckForHiddenObject)
	ld [MBC1RomBank], a
	ldh [hLoadedROMBank], a
	call CheckForHiddenObject
	ldh a, [hDidntFindAnyHiddenObject]
	and a
	jr nz, .hiddenObjectNotFound
	ld a, [wHiddenObjectFunctionRomBank]
	ld [MBC1RomBank], a
	ldh [hLoadedROMBank], a
	ld de, .returnAddress
	push de
	jp hl
.returnAddress
	xor a
	jr .done
.hiddenObjectNotFound
	farcall PrintBookshelfText
	ldh a, [hFFDB]
	and a
	jr z, .done
.nothingFound
	ld a, $ff
.done
	ldh [hItemAlreadyFound], a
	pop af
	ld [MBC1RomBank], a
	ldh [hLoadedROMBank], a
	ret

PrintPredefTextID::
	ldh [hSpriteIndexOrTextID], a
	ld hl, TextPredefs
	call SetMapTextPointer
	ld hl, wTextPredefFlag
	set 0, [hl]
	call DisplayTextID

RestoreMapTextPointer::
	ld hl, wMapTextPtr
	ldh a, [hSavedMapTextPtr]
	ld [hli], a
	ldh a, [hSavedMapTextPtr + 1]
	ld [hl], a
	ret

SetMapTextPointer::
	ld a, [wMapTextPtr]
	ldh [hSavedMapTextPtr], a
	ld a, [wMapTextPtr + 1]
	ldh [hSavedMapTextPtr + 1], a
	ld a, l
	ld [wMapTextPtr], a
	ld a, h
	ld [wMapTextPtr + 1], a
	ret

INCLUDE "data/text_predef_pointers.asm"

