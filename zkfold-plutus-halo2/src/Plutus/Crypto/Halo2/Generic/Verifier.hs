{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -ddump-splices #-}
-- no-unused-local-binds is here because for some circuits not all bindings are used
-- and unused bindings are eliminated by plutus compiler
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{-# HLINT ignore "Use camelCase" #-}

module Plutus.Crypto.Halo2.Generic.Verifier (verify) where

import           Language.Haskell.TH.Syntax                       (lift)
import           Plutus.Crypto.BlsTypes                           (MultiplicativeGroup (recip), Scalar, mkScalar,
                                                                   powMod)
import qualified Plutus.Crypto.BlsUtils                           as BlsUtils
import qualified Plutus.Crypto.Constants                          as Constants
import           Plutus.Crypto.Halo2                              (Proof, bls12_381_field_prime)
import qualified Plutus.Crypto.Halo2.ApplicativeParser            as M
import qualified Plutus.Crypto.Halo2.Generic.VKConstants          as VKConstants
import           Plutus.Crypto.Halo2.Halo2MultiOpenMSM            (buildMSM)
import           Plutus.Crypto.Halo2.LagrangePolynomialEvaluation (lagrangePolynomialBasis)
import           Plutus.Crypto.Halo2.MSMEval                      (eval)
import           Plutus.Crypto.Orphans                            ()
import           PlutusTx.Builtins                                (BuiltinBLS12_381_G1_Element,
                                                                   BuiltinBLS12_381_G2_Element,
                                                                   BuiltinBLS12_381_MlResult, BuiltinByteString,
                                                                   bls12_381_G1_compressed_zero,
                                                                   bls12_381_G1_uncompress,
                                                                   bls12_381_G2_compressed_generator,
                                                                   bls12_381_G2_uncompress, bls12_381_finalVerify,
                                                                   bls12_381_millerLoop)
import           PlutusTx.List                                    (drop, foldl, head, take, (!!))
import           PlutusTx.Prelude                                 (AdditiveGroup (..), AdditiveSemigroup (..), Bool,
                                                                   Integer, MultiplicativeSemigroup (..), flip, fst,
                                                                   modulo, negate, scale, zero, ($))

{-# INLINEABLE innerProduct #-}
innerProduct :: [Scalar] -> [Scalar] -> Scalar
innerProduct [] []             = mkScalar 0
innerProduct (x : xs) (y : ys) = (x * y) + (innerProduct xs ys)
-- todo throw here as lists are of different sizes
innerProduct _ _               = mkScalar 0

-- FROM VERIFICATION KEY

scalarDelta :: Scalar
scalarDelta = $(lift Constants.scalarDelta)

scalarZero :: Scalar
scalarZero = $(lift Constants.scalarZero)

scalarOne :: Scalar
scalarOne = $(lift Constants.scalarOne)

omega :: Scalar
omega = $(lift VKConstants.omega_val)

omegaInv :: Scalar
omegaInv = $(lift VKConstants.omegaInv_val)

barycentricWeight :: Scalar
barycentricWeight = $(lift VKConstants.barycentricWeight_val)

s_g2 :: BuiltinBLS12_381_G2_Element
s_g2 = $(lift VKConstants.s_g2_val)

f1_commitment :: BuiltinBLS12_381_G1_Element
f1_commitment = $(lift VKConstants.f1_commitment)

f2_commitment :: BuiltinBLS12_381_G1_Element
f2_commitment = $(lift VKConstants.f2_commitment)

f3_commitment :: BuiltinBLS12_381_G1_Element
f3_commitment = $(lift VKConstants.f3_commitment)

f4_commitment :: BuiltinBLS12_381_G1_Element
f4_commitment = $(lift VKConstants.f4_commitment)

f5_commitment :: BuiltinBLS12_381_G1_Element
f5_commitment = $(lift VKConstants.f5_commitment)

f6_commitment :: BuiltinBLS12_381_G1_Element
f6_commitment = $(lift VKConstants.f6_commitment)

f7_commitment :: BuiltinBLS12_381_G1_Element
f7_commitment = $(lift VKConstants.f7_commitment)

f8_commitment :: BuiltinBLS12_381_G1_Element
f8_commitment = $(lift VKConstants.f8_commitment)

f9_commitment :: BuiltinBLS12_381_G1_Element
f9_commitment = $(lift VKConstants.f9_commitment)



p1_commitment :: BuiltinBLS12_381_G1_Element
p1_commitment = $(lift VKConstants.p1_commitment)

p2_commitment :: BuiltinBLS12_381_G1_Element
p2_commitment = $(lift VKConstants.p2_commitment)

p3_commitment :: BuiltinBLS12_381_G1_Element
p3_commitment = $(lift VKConstants.p3_commitment)

p4_commitment :: BuiltinBLS12_381_G1_Element
p4_commitment = $(lift VKConstants.p4_commitment)



rotations_for_instances :: [Scalar]
rotations_for_instances = $(lift (BlsUtils.getRotatedOmegas VKConstants.omega_val VKConstants.omegaInv_val 0 40))

rotations_for_vanishing :: [Scalar]
rotations_for_vanishing =
  $( lift
      ( BlsUtils.getRotatedOmegas
          VKConstants.omega_val
          VKConstants.omegaInv_val
          (-(VKConstants.blinding_factors + 1))
          0
      )
   )

{-# INLINEABLE verify #-}
verify :: Proof -> [Scalar] -> (Bool, [(BuiltinByteString, BlsUtils.Tracing)])
verify proof [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, p37, p38, p39, p40] = fst $ flip (M.run VKConstants.transcriptRepr) proof $ M.do
  --  public inputs
  _ <- M.commonScalar (mkScalar 40)
  !i1 <- M.commonScalar p1
  !i2 <- M.commonScalar p2
  !i3 <- M.commonScalar p3
  !i4 <- M.commonScalar p4
  !i5 <- M.commonScalar p5
  !i6 <- M.commonScalar p6
  !i7 <- M.commonScalar p7
  !i8 <- M.commonScalar p8
  !i9 <- M.commonScalar p9
  !i10 <- M.commonScalar p10
  !i11 <- M.commonScalar p11
  !i12 <- M.commonScalar p12
  !i13 <- M.commonScalar p13
  !i14 <- M.commonScalar p14
  !i15 <- M.commonScalar p15
  !i16 <- M.commonScalar p16
  !i17 <- M.commonScalar p17
  !i18 <- M.commonScalar p18
  !i19 <- M.commonScalar p19
  !i20 <- M.commonScalar p20
  !i21 <- M.commonScalar p21
  !i22 <- M.commonScalar p22
  !i23 <- M.commonScalar p23
  !i24 <- M.commonScalar p24
  !i25 <- M.commonScalar p25
  !i26 <- M.commonScalar p26
  !i27 <- M.commonScalar p27
  !i28 <- M.commonScalar p28
  !i29 <- M.commonScalar p29
  !i30 <- M.commonScalar p30
  !i31 <- M.commonScalar p31
  !i32 <- M.commonScalar p32
  !i33 <- M.commonScalar p33
  !i34 <- M.commonScalar p34
  !i35 <- M.commonScalar p35
  !i36 <- M.commonScalar p36
  !i37 <- M.commonScalar p37
  !i38 <- M.commonScalar p38
  !i39 <- M.commonScalar p39
  !i40 <- M.commonScalar p40


  !a1 <- M.readPoint
  !a2 <- M.readPoint
  !a3 <- M.readPoint
  !theta <- M.squeezeChallenge
  !permutedInput1 <- M.readPoint
  !permutedTable1 <- M.readPoint
  !beta <- M.squeezeChallenge
  !gamma <- M.squeezeChallenge
  !permutations_committed_a <- M.readPoint
  !permutations_committed_b <- M.readPoint
  !lookupCommitment1 <- M.readPoint
  !vanishingRand <- M.readPoint
  !y <- M.squeezeChallenge
  !vanishingSplit_1 <- M.readPoint
  !vanishingSplit_2 <- M.readPoint
  !vanishingSplit_3 <- M.readPoint
  !vanishingSplit_4 <- M.readPoint
  !x <- M.squeezeChallenge
  !adviceEval1 <- M.readScalar
  !adviceEval2 <- M.readScalar
  !adviceEval3 <- M.readScalar
  !fixedEval1 <- M.readScalar
  !fixedEval2 <- M.readScalar
  !fixedEval3 <- M.readScalar
  !fixedEval4 <- M.readScalar
  !fixedEval5 <- M.readScalar
  !fixedEval6 <- M.readScalar
  !fixedEval7 <- M.readScalar
  !fixedEval8 <- M.readScalar
  !fixedEval9 <- M.readScalar
  !randomEval <- M.readScalar
  !permutationCommon1 <- M.readScalar
  !permutationCommon2 <- M.readScalar
  !permutationCommon3 <- M.readScalar
  !permutationCommon4 <- M.readScalar
  !permutations_evaluated_a_1 <- M.readScalar
  !permutations_evaluated_a_2 <- M.readScalar
  !permutations_evaluated_a_3 <- M.readScalar
  !permutations_evaluated_b_1 <- M.readScalar
  !permutations_evaluated_b_2 <- M.readScalar
  !product_eval_1 <- M.readScalar
  !product_next_eval_1 <- M.readScalar
  !permuted_input_eval_1 <- M.readScalar
  !permuted_input_inv_eval_1 <- M.readScalar
  !permuted_table_eval_1 <- M.readScalar
  !x1 <- M.squeezeChallenge
  !x2 <- M.squeezeChallenge
  !f_commitment <- M.readPoint
  !x3 <- M.squeezeChallenge
  !q_eval_on_x3_1 <- M.readScalar
  !q_eval_on_x3_2 <- M.readScalar
  !q_eval_on_x3_3 <- M.readScalar
  !q_eval_on_x3_4 <- M.readScalar
  !x4 <- M.squeezeChallenge
  !pi_term <- M.readPoint


  let !rotateOmega = BlsUtils.rotateOmega omega omegaInv
      !n = 2097152
      !xn = powMod x n

-- todo check if there are case where more X rotations are needed
      !x_prev = rotateOmega x (-1)
      !x_current = rotateOmega x 0
      !x_next = rotateOmega x 1
      !x_last = rotateOmega x (-(VKConstants.blinding_factors + 1))

      --    lagrange eval for instances (public inputs)
      !lagrange_polynomial_instances = lagrangePolynomialBasis x xn barycentricWeight rotations_for_instances

      !instanceEval1 = innerProduct lagrange_polynomial_instances [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40]

      !gate_eq1 = ((((((fixedEval1 * adviceEval1) * adviceEval2) + (fixedEval2 * adviceEval1)) + (fixedEval3 * adviceEval2)) + (fixedEval4 * adviceEval3)) + fixedEval5)


      !lookup_table_eq1 = (((scalarZero * theta + fixedEval7) * theta + fixedEval8) * theta + fixedEval9)

      !lookup_input_eq1 = (((scalarZero * theta + (fixedEval6 * adviceEval1)) * theta + (fixedEval6 * adviceEval2)) * theta + (fixedEval6 * adviceEval3))


      --    lagrange eval for vanishing polynomial
      !evaluations_of_lagrange_polynomial =
        lagrangePolynomialBasis
          x
          xn
          barycentricWeight
          rotations_for_vanishing
      !last_evaluation = head evaluations_of_lagrange_polynomial
      !evaluation_for_blinding_factors = take VKConstants.blinding_factors (drop 1 evaluations_of_lagrange_polynomial)
      !sum_of_evaluation_for_blinding_factors = foldl (+) zero evaluation_for_blinding_factors
      !evaluation_at_0 = evaluations_of_lagrange_polynomial !! (1 + VKConstants.blinding_factors)

      !term1 = ( evaluation_at_0  * ( scalarOne  + ( negate  permutations_evaluated_a_1  )))
      !term2 = ( last_evaluation  * (( permutations_evaluated_b_1  *  permutations_evaluated_b_1 ) + ( negate  permutations_evaluated_b_1  )))
      !term3 = (( permutations_evaluated_b_1  + ( negate  permutations_evaluated_a_3  )) *  evaluation_at_0 )


      !left1 = ((adviceEval1 + ( beta  * permutationCommon1)) +  gamma ) --part of set a
      !left2 = ((adviceEval2 + ( beta  * permutationCommon2)) +  gamma ) --part of set a
      !left3 = ((adviceEval3 + ( beta  * permutationCommon3)) +  gamma ) --part of set a
      !left4 = ((instanceEval1 + ( beta  * permutationCommon4)) +  gamma ) --part of set b


      !left_set1 = permutations_evaluated_a_2 * left1 * left2 * left3
      !left_set2 = permutations_evaluated_b_2 * left4


      !right1 = ((adviceEval1 + (( beta  *  x ) * ( powMod  scalarDelta  0  ))) +  gamma ) --part of set a
      !right2 = ((adviceEval2 + (( beta  *  x ) * ( powMod  scalarDelta  1  ))) +  gamma ) --part of set a
      !right3 = ((adviceEval3 + (( beta  *  x ) * ( powMod  scalarDelta  2  ))) +  gamma ) --part of set a
      !right4 = ((instanceEval1 + (( beta  *  x ) * ( powMod  scalarDelta  3  ))) +  gamma ) --part of set b


      !right_set1 = permutations_evaluated_a_1 * right1 * right2 * right3
      !right_set2 = permutations_evaluated_b_1 * right4


      !permutations1 = (left_set1 - right_set1) * (scalarOne - (last_evaluation + sum_of_evaluation_for_blinding_factors))
      !permutations2 = (left_set2 - right_set2) * (scalarOne - (last_evaluation + sum_of_evaluation_for_blinding_factors))


      -- lookups related part of vanishing polynomial
      !active_rows = scalarOne - (last_evaluation + sum_of_evaluation_for_blinding_factors);

      !lookup_expression_1_1 = evaluation_at_0 * (scalarOne - product_eval_1)
      !lookup_expression_2_1 = last_evaluation * (product_eval_1 * product_eval_1 - product_eval_1)
      !lookup_left_1 = product_next_eval_1 * (permuted_input_eval_1 + beta) * (permuted_table_eval_1 + gamma)
      !lookup_right_1 = product_eval_1 * (lookup_input_eq1 + beta) * (lookup_table_eq1 + gamma)
      !lookup_expression_3_1 = (lookup_left_1 - lookup_right_1) * active_rows
      !lookup_expression_4_1 = evaluation_at_0 * (permuted_input_eval_1 - permuted_table_eval_1)
      !lookup_expression_5_1 = (permuted_input_eval_1 - permuted_table_eval_1) * (permuted_input_eval_1 - permuted_input_inv_eval_1) * active_rows




      --      expressions for vanishing polynomial
      !expression1 = gate_eq1
      !expression2 = term1
      !expression3 = term2
      !expression4 = term3
      !expression5 = permutations1
      !expression6 = permutations2
      !expression7 = lookup_expression_1_1
      !expression8 = lookup_expression_2_1
      !expression9 = lookup_expression_3_1
      !expression10 = lookup_expression_4_1
      !expression11 = lookup_expression_5_1


      !hEval = (((((((((((scalarZero * y + expression1) * y + expression2) * y + expression3) * y + expression4) * y + expression5) * y + expression6) * y + expression7) * y + expression8) * y + expression9) * y + expression10) * y + expression11)


      !vanishing_s = hEval * recip (xn - scalarOne) -- recip is inverse of the number mod bls12_381_field_prime

      -- vanishing split IS PASSED IN REVERSE ORDER!!!
      -- this is MSM done for h-commitments
      !hCommitment1 = ( (scale  xn   (bls12_381_G1_uncompress bls12_381_G1_compressed_zero) ) +  vanishingSplit_4 )
      !hCommitment2 = ( (scale  xn   hCommitment1 ) +  vanishingSplit_3 )
      !hCommitment3 = ( (scale  xn   hCommitment2 ) +  vanishingSplit_2 )
      !vanishing_g = ( (scale  xn   hCommitment3 ) +  vanishingSplit_1 )


      -- commitment map, tuples are (commitment, point_set_index, points, evaluations)
      commitment_data :: [(BuiltinBLS12_381_G1_Element, Integer, [Scalar], [Scalar])]
      !commitment_data = [(a1, 0, [x_current], [adviceEval1]),(a2, 0, [x_current], [adviceEval2]),(a3, 0, [x_current], [adviceEval3]),(permutations_committed_a, 1, [x_current,x_next,x_last], [permutations_evaluated_a_1,permutations_evaluated_a_2,permutations_evaluated_a_3]),(permutations_committed_b, 2, [x_current,x_next], [permutations_evaluated_b_1,permutations_evaluated_b_2]),(lookupCommitment1, 2, [x_current,x_next], [product_eval_1,product_next_eval_1]),(permutedInput1, 3, [x_current,x_prev], [permuted_input_eval_1,permuted_input_inv_eval_1]),(permutedTable1, 0, [x_current], [permuted_table_eval_1]),(f1_commitment, 0, [x_current], [fixedEval1]),(f2_commitment, 0, [x_current], [fixedEval2]),(f3_commitment, 0, [x_current], [fixedEval3]),(f4_commitment, 0, [x_current], [fixedEval4]),(f5_commitment, 0, [x_current], [fixedEval5]),(f6_commitment, 0, [x_current], [fixedEval6]),(f7_commitment, 0, [x_current], [fixedEval7]),(f8_commitment, 0, [x_current], [fixedEval8]),(f9_commitment, 0, [x_current], [fixedEval9]),(p1_commitment, 0, [x_current], [permutationCommon1]),(p2_commitment, 0, [x_current], [permutationCommon2]),(p3_commitment, 0, [x_current], [permutationCommon3]),(p4_commitment, 0, [x_current], [permutationCommon4]),(vanishing_g, 0, [x_current], [vanishing_s]),(vanishingRand, 0, [x_current], [randomEval])]

      -- point sets with correct order of sets in list
      !point_sets = [[x_current],[x_current,x_next,x_last],[x_current,x_next],[x_current,x_prev]]

      -- left and s_g2
      -- right and g2
      -- g2 is not negated as bls12_381_finalVerify is doing negation
      !g2 = (bls12_381_G2_uncompress bls12_381_G2_compressed_generator)

      !x1Powers = BlsUtils.powers 19 x1
      !x4Powers = BlsUtils.powers 5 x4

      !alt_right =
          buildMSM
              x1Powers
              x2
              x3
              x4Powers
              f_commitment
              pi_term
              [q_eval_on_x3_1, q_eval_on_x3_2, q_eval_on_x3_3, q_eval_on_x3_4]
              commitment_data
              point_sets

      !el = pi_term
      !er = eval alt_right

      ml_l :: BuiltinBLS12_381_MlResult
      !ml_l = bls12_381_millerLoop el s_g2
      ml_r :: BuiltinBLS12_381_MlResult
      !ml_r = bls12_381_millerLoop er g2
      final_verification :: Bool
      !final_verification = bls12_381_finalVerify ml_l ml_r

  M.return
    ( final_verification,
      -- tracing related values
      []
    )
