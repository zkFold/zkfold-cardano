// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "./AsterizmHashLib.sol";

contract HashHarness {
    // Call with the full preimage: header (112 bytes) + payload (>0 bytes).
    function buildCrossRaw(bytes memory packed) public pure returns (bytes32) {
        require(packed.length > 112, "packed must be > 112 bytes");
        return AsterizmHashLib.buildCrosschainHash(packed);
    }
}