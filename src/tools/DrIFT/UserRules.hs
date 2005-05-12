-- stub module to add your own rules.
module UserRules where

import StandardRules(Rule,Tag) -- gives some examples 
import RuleUtils -- useful to have a look at this too

import UserRuleBinary
import UserRuleXml

-- add your rules to this list
userRules :: [Rule]
userRules = [("Haskell2Xml", userRuleXml), ("Binary", userRuleBinary)]

