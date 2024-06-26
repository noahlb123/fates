module FatesPyrogenicCarbonMod
  ! -------------------------------------------------------------------------------------
  ! This module contains type and definitions for pyrogenic carbon
  !
  ! In FATES, Pyrogenic carbon is any carbon that is both 1. changed by fire and 2.
  ! relatively stationary. This includes charcoal and some ash, but excludes
  ! gaseous fire products and fire aerosols.
  ! -------------------------------------------------------------------------------------

  use FatesConstantsMod,           only : r8 => fates_r8
  use FatesLitterMod,              only : nfsc
  use FatesConstantsMod,           only : i8 => fates_int

  implicit none
  private

  ! Pyrogenic Carbon Production Factors (grams PyC / grams carbon burned)
  ! for each litter type (twigs, small branches, large branches, trunks)
  real(r8), parameter, public :: pyc_proc_facs(ncwd) = &
  (/0.099_r8, 0.1095_r8, 0.12_r8, 0.25_r8/)
  
  !PyC loss factors
  real(r8), parameter, public :: pyc_fire_loss = 0.901_r8
  real(r8), parameter, public :: pyc_daily_loss = 0.9994521_r8

contains

  !calculate pyc from burnt living leaves and grass
  !Notice diff with PycLeafLitter is that these leaves were a part of living trees before fire
  subroutine PycLivingLeavesGrass(currentPatch, leaf_burn_frac, leaf_m, n, is_woody)

     type(fates_patch_type),intent(inout),target :: currentPatch
     integer,intent(in)                          :: n                 ! number of plants burned
     real(r8),intent(in)                         :: leaf_burn_frac
     real(r8),intent(in)                         :: leaf_m            ! kg
     logical,intent(in)                          :: is_woody          ! is woody or is grass

     if (is_woody) then !burn living leaves
        currentPatch%pyrogenic_carbon(5) = currentPatch%pyrogenic_carbon(5) + 0.03488333_r8 * leaf_burn_frac * leaf_m * n
     else               !burn living grass 
        currentPatch%pyrogenic_carbon(6) = currentPatch%pyrogenic_carbon(6) + 0.03137333_r8 * leaf_burn_frac * leaf_m * n
     endif

     return
  end subroutine PycLivingLeavesGrass



  !pyc from plants burned that were already dead before fire
  subroutine PycDeadPlants(currentPatch, fraction_crown_burned, area, repro_m, leaf_m, num_dead_trees, is_woody)

     type(fates_patch_type),intent(inout),target :: currentPatch
     integer,intent(in)                          :: num_dead_trees
     real(r8),intent(in)                         :: fraction_crown_burned
     real(r8),intent(in)                         :: area
     real(r8),intent(in)                         :: leaf_m            ! leaf mass
     real(r8),intent(in)                         :: repro_m           ! reprodictive tissue mass
     logical,intent(in)                          :: is_woody          ! is woody or is grass

     if (is_woody) then !burn living leaves
        currentPatch%pyrogenic_carbon(4) = currentPatch%pyrogenic_carbon(4) + num_dead_trees * (leaf_m+repro_m) * fraction_crown_burned * 0.0136_r8 * area
     else               !burn living grass 
        currentPatch%pyrogenic_carbon(6) = currentPatch%pyrogenic_carbon(6) + num_dead_trees * leaf_m * fraction_crown_burned * 0.0313733_r8 * area
     endif

     !pyc = burned_mass*pyc_fact
     !burned_mass = max(0.0, burned_mass - pyc)

     return
  end subroutine PycDeadPlants



  !pyc from woody debris
  subroutine PycWoodyDebris(currentPatch, burned_mass, retain_m2, donate_m2, c)

     type(fates_patch_type),intent(inout),target :: currentPatch
     integer,intent(in)                          :: c             ! woody debris type index
     real(r8),intent(in)                         :: burned_mass
     real(r8),intent(in)                         :: retain_m2     ! retained area after fire
     real(r8),intent(in)                         :: donate_m2     ! donated area after fire

     !burned_mass = max(0.0, burned_mass - pyc)

     ! transfer pyc between patches
     currentPatch%pyrogenic_carbon(c) = currentPatch%pyrogenic_carbon(c) + burned_mass * pyc_proc_facs(c) * retain_m2
     newPatch%pyrogenic_carbon(c) = newPatch%pyrogenic_carbon(c) + burned_mass * pyc_proc_facs(c) * donate_m2

     return
  end subroutine PycLivingLeavesGrass



  !pyc from leaf litter
  subroutine PycLeafLitter(currentPatch, burned_mass, retain_m2, donate_m2)

     type(fates_patch_type),intent(inout),target :: currentPatch
     integer,intent(in)                          :: c             ! woody debris type index 
     real(r8),intent(in)                         :: burned_mass   
     real(r8),intent(in)                         :: retain_m2     ! retained area after fire 
     real(r8),intent(in)                         :: donate_m2     ! donated area after fire

     !burned_mass = max(0.0, burned_mass - pyc)

     ! transfer pyc between patches
     currentPatch%pyrogenic_carbon(5) = currentPatch%pyrogenic_carbon(5) + burned_mass * 0.01602_r8 * retain_m2
     newPatch%pyrogenic_carbon(5) = newPatch%pyrogenic_carbon(5) + burned_mass * 0.01602_r8 * donate_m2

     return
  end subroutine PycLivingLeavesGrass



  !some pyc is itself burnt during a fire
  subroutine PycFireLoss(currentPatch)

     type(fates_patch_type),intent(inout),target :: currentPatch

     currentPatch%pyrogenic_carbon(:) = pyc_fire_loss * currentPatch%pyrogenic_carbon(:)

     return
  end subroutine PycLivingLeavesGrass



  !pyc is degraded very slowly at daily timestep
  subroutine PycDailyLoss(currentPatch)

     type(fates_patch_type),intent(inout),target :: currentPatch

     currentPatch%pyrogenic_carbon(:) = pyc_daily_loss * currentPatch%pyrogenic_carbon(:)

     return
  end subroutine PycLivingLeavesGrass
end module FatesPyrogenicCarbonMod
