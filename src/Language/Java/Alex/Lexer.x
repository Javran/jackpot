{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Language.Java.Alex.Lexer where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as BSL
import Language.Java.Alex.Token
import {-# SOURCE #-} Language.Java.Alex.Alex
}

$digit = 0-9
$hexdigit = [0-9a-fA-F]
$alpha = [a-zA-Z]

-- WhiteSpace in Java is:
-- SP (\ ) HT (\t) FF (\f)
-- CR (\r) LF (\n) but $white includes VT (\v) and we need to exclude that.
$JavaWhiteSpace = $white # \v

-- Note that Alex excludes `\n` by default, so we have to be explicit about it.
$NotStar = [.\n] # \*
$NotStarNotSlash = $NotStar # \/
$NotDbQuoteNotSlash = [.\n] # \" # \/

-- See: [Regarding $JavaIdentifierStart and $JavaIdentifierPart] section in /docs/README.md
$JavaIdentifierStart = [\x24\x41-\x5A\x5F\x61-\x7A\xA2-\xA5\xAA\xB5\xBA\xC0-\xD6\xD8-\xF6\xF8-\x2C1\x2C6-\x2D1\x2E0-\x2E4\x2EC\x2EE\x370-\x374\x376-\x377\x37A-\x37D\x37F\x386\x388-\x38A\x38C\x38E-\x3A1\x3A3-\x3F5\x3F7-\x481\x48A-\x52F\x531-\x556\x559\x560-\x588\x58F\x5D0-\x5EA\x5EF-\x5F2\x60B\x620-\x64A\x66E-\x66F\x671-\x6D3\x6D5\x6E5-\x6E6\x6EE-\x6EF\x6FA-\x6FC\x6FF\x710\x712-\x72F\x74D-\x7A5\x7B1\x7CA-\x7EA\x7F4-\x7F5\x7FA\x7FE-\x815\x81A\x824\x828\x840-\x858\x860-\x86A\x8A0-\x8B4\x8B6-\x8C7\x904-\x939\x93D\x950\x958-\x961\x971-\x980\x985-\x98C\x98F-\x990\x993-\x9A8\x9AA-\x9B0\x9B2\x9B6-\x9B9\x9BD\x9CE\x9DC-\x9DD\x9DF-\x9E1\x9F0-\x9F3\x9FB-\x9FC\xA05-\xA0A\xA0F-\xA10\xA13-\xA28\xA2A-\xA30\xA32-\xA33\xA35-\xA36\xA38-\xA39\xA59-\xA5C\xA5E\xA72-\xA74\xA85-\xA8D\xA8F-\xA91\xA93-\xAA8\xAAA-\xAB0\xAB2-\xAB3\xAB5-\xAB9\xABD\xAD0\xAE0-\xAE1\xAF1\xAF9\xB05-\xB0C\xB0F-\xB10\xB13-\xB28\xB2A-\xB30\xB32-\xB33\xB35-\xB39\xB3D\xB5C-\xB5D\xB5F-\xB61\xB71\xB83\xB85-\xB8A\xB8E-\xB90\xB92-\xB95\xB99-\xB9A\xB9C\xB9E-\xB9F\xBA3-\xBA4\xBA8-\xBAA\xBAE-\xBB9\xBD0\xBF9\xC05-\xC0C\xC0E-\xC10\xC12-\xC28\xC2A-\xC39\xC3D\xC58-\xC5A\xC60-\xC61\xC80\xC85-\xC8C\xC8E-\xC90\xC92-\xCA8\xCAA-\xCB3\xCB5-\xCB9\xCBD\xCDE\xCE0-\xCE1\xCF1-\xCF2\xD04-\xD0C\xD0E-\xD10\xD12-\xD3A\xD3D\xD4E\xD54-\xD56\xD5F-\xD61\xD7A-\xD7F\xD85-\xD96\xD9A-\xDB1\xDB3-\xDBB\xDBD\xDC0-\xDC6\xE01-\xE30\xE32-\xE33\xE3F-\xE46\xE81-\xE82\xE84\xE86-\xE8A\xE8C-\xEA3\xEA5\xEA7-\xEB0\xEB2-\xEB3\xEBD\xEC0-\xEC4\xEC6\xEDC-\xEDF\xF00\xF40-\xF47\xF49-\xF6C\xF88-\xF8C\x1000-\x102A\x103F\x1050-\x1055\x105A-\x105D\x1061\x1065-\x1066\x106E-\x1070\x1075-\x1081\x108E\x10A0-\x10C5\x10C7\x10CD\x10D0-\x10FA\x10FC-\x1248\x124A-\x124D\x1250-\x1256\x1258\x125A-\x125D\x1260-\x1288\x128A-\x128D\x1290-\x12B0\x12B2-\x12B5\x12B8-\x12BE\x12C0\x12C2-\x12C5\x12C8-\x12D6\x12D8-\x1310\x1312-\x1315\x1318-\x135A\x1380-\x138F\x13A0-\x13F5\x13F8-\x13FD\x1401-\x166C\x166F-\x167F\x1681-\x169A\x16A0-\x16EA\x16EE-\x16F8\x1700-\x170C\x170E-\x1711\x1720-\x1731\x1740-\x1751\x1760-\x176C\x176E-\x1770\x1780-\x17B3\x17D7\x17DB-\x17DC\x1820-\x1878\x1880-\x1884\x1887-\x18A8\x18AA\x18B0-\x18F5\x1900-\x191E\x1950-\x196D\x1970-\x1974\x1980-\x19AB\x19B0-\x19C9\x1A00-\x1A16\x1A20-\x1A54\x1AA7\x1B05-\x1B33\x1B45-\x1B4B\x1B83-\x1BA0\x1BAE-\x1BAF\x1BBA-\x1BE5\x1C00-\x1C23\x1C4D-\x1C4F\x1C5A-\x1C7D\x1C80-\x1C88\x1C90-\x1CBA\x1CBD-\x1CBF\x1CE9-\x1CEC\x1CEE-\x1CF3\x1CF5-\x1CF6\x1CFA\x1D00-\x1DBF\x1E00-\x1F15\x1F18-\x1F1D\x1F20-\x1F45\x1F48-\x1F4D\x1F50-\x1F57\x1F59\x1F5B\x1F5D\x1F5F-\x1F7D\x1F80-\x1FB4\x1FB6-\x1FBC\x1FBE\x1FC2-\x1FC4\x1FC6-\x1FCC\x1FD0-\x1FD3\x1FD6-\x1FDB\x1FE0-\x1FEC\x1FF2-\x1FF4\x1FF6-\x1FFC\x203F-\x2040\x2054\x2071\x207F\x2090-\x209C\x20A0-\x20BF\x2102\x2107\x210A-\x2113\x2115\x2119-\x211D\x2124\x2126\x2128\x212A-\x212D\x212F-\x2139\x213C-\x213F\x2145-\x2149\x214E\x2160-\x2188\x2C00-\x2C2E\x2C30-\x2C5E\x2C60-\x2CE4\x2CEB-\x2CEE\x2CF2-\x2CF3\x2D00-\x2D25\x2D27\x2D2D\x2D30-\x2D67\x2D6F\x2D80-\x2D96\x2DA0-\x2DA6\x2DA8-\x2DAE\x2DB0-\x2DB6\x2DB8-\x2DBE\x2DC0-\x2DC6\x2DC8-\x2DCE\x2DD0-\x2DD6\x2DD8-\x2DDE\x2E2F\x3005-\x3007\x3021-\x3029\x3031-\x3035\x3038-\x303C\x3041-\x3096\x309D-\x309F\x30A1-\x30FA\x30FC-\x30FF\x3105-\x312F\x3131-\x318E\x31A0-\x31BF\x31F0-\x31FF\x3400-\x4DBF\x4E00-\x9FFC\xA000-\xA48C\xA4D0-\xA4FD\xA500-\xA60C\xA610-\xA61F\xA62A-\xA62B\xA640-\xA66E\xA67F-\xA69D\xA6A0-\xA6EF\xA717-\xA71F\xA722-\xA788\xA78B-\xA7BF\xA7C2-\xA7CA\xA7F5-\xA801\xA803-\xA805\xA807-\xA80A\xA80C-\xA822\xA838\xA840-\xA873\xA882-\xA8B3\xA8F2-\xA8F7\xA8FB\xA8FD-\xA8FE\xA90A-\xA925\xA930-\xA946\xA960-\xA97C\xA984-\xA9B2\xA9CF\xA9E0-\xA9E4\xA9E6-\xA9EF\xA9FA-\xA9FE\xAA00-\xAA28\xAA40-\xAA42\xAA44-\xAA4B\xAA60-\xAA76\xAA7A\xAA7E-\xAAAF\xAAB1\xAAB5-\xAAB6\xAAB9-\xAABD\xAAC0\xAAC2\xAADB-\xAADD\xAAE0-\xAAEA\xAAF2-\xAAF4\xAB01-\xAB06\xAB09-\xAB0E\xAB11-\xAB16\xAB20-\xAB26\xAB28-\xAB2E\xAB30-\xAB5A\xAB5C-\xAB69\xAB70-\xABE2\xAC00-\xD7A3\xD7B0-\xD7C6\xD7CB-\xD7FB\xF900-\xFA6D\xFA70-\xFAD9\xFB00-\xFB06\xFB13-\xFB17\xFB1D\xFB1F-\xFB28\xFB2A-\xFB36\xFB38-\xFB3C\xFB3E\xFB40-\xFB41\xFB43-\xFB44\xFB46-\xFBB1\xFBD3-\xFD3D\xFD50-\xFD8F\xFD92-\xFDC7\xFDF0-\xFDFC\xFE33-\xFE34\xFE4D-\xFE4F\xFE69\xFE70-\xFE74\xFE76-\xFEFC\xFF04\xFF21-\xFF3A\xFF3F\xFF41-\xFF5A\xFF66-\xFFBE\xFFC2-\xFFC7\xFFCA-\xFFCF\xFFD2-\xFFD7\xFFDA-\xFFDC\xFFE0-\xFFE1\xFFE5-\xFFE6\x10000-\x1000B\x1000D-\x10026\x10028-\x1003A\x1003C-\x1003D\x1003F-\x1004D\x10050-\x1005D\x10080-\x100FA\x10140-\x10174\x10280-\x1029C\x102A0-\x102D0\x10300-\x1031F\x1032D-\x1034A\x10350-\x10375\x10380-\x1039D\x103A0-\x103C3\x103C8-\x103CF\x103D1-\x103D5\x10400-\x1049D\x104B0-\x104D3\x104D8-\x104FB\x10500-\x10527\x10530-\x10563\x10600-\x10736\x10740-\x10755\x10760-\x10767\x10800-\x10805\x10808\x1080A-\x10835\x10837-\x10838\x1083C\x1083F-\x10855\x10860-\x10876\x10880-\x1089E\x108E0-\x108F2\x108F4-\x108F5\x10900-\x10915\x10920-\x10939\x10980-\x109B7\x109BE-\x109BF\x10A00\x10A10-\x10A13\x10A15-\x10A17\x10A19-\x10A35\x10A60-\x10A7C\x10A80-\x10A9C\x10AC0-\x10AC7\x10AC9-\x10AE4\x10B00-\x10B35\x10B40-\x10B55\x10B60-\x10B72\x10B80-\x10B91\x10C00-\x10C48\x10C80-\x10CB2\x10CC0-\x10CF2\x10D00-\x10D23\x10E80-\x10EA9\x10EB0-\x10EB1\x10F00-\x10F1C\x10F27\x10F30-\x10F45\x10FB0-\x10FC4\x10FE0-\x10FF6\x11003-\x11037\x11083-\x110AF\x110D0-\x110E8\x11103-\x11126\x11144\x11147\x11150-\x11172\x11176\x11183-\x111B2\x111C1-\x111C4\x111DA\x111DC\x11200-\x11211\x11213-\x1122B\x11280-\x11286\x11288\x1128A-\x1128D\x1128F-\x1129D\x1129F-\x112A8\x112B0-\x112DE\x11305-\x1130C\x1130F-\x11310\x11313-\x11328\x1132A-\x11330\x11332-\x11333\x11335-\x11339\x1133D\x11350\x1135D-\x11361\x11400-\x11434\x11447-\x1144A\x1145F-\x11461\x11480-\x114AF\x114C4-\x114C5\x114C7\x11580-\x115AE\x115D8-\x115DB\x11600-\x1162F\x11644\x11680-\x116AA\x116B8\x11700-\x1171A\x11800-\x1182B\x118A0-\x118DF\x118FF-\x11906\x11909\x1190C-\x11913\x11915-\x11916\x11918-\x1192F\x1193F\x11941\x119A0-\x119A7\x119AA-\x119D0\x119E1\x119E3\x11A00\x11A0B-\x11A32\x11A3A\x11A50\x11A5C-\x11A89\x11A9D\x11AC0-\x11AF8\x11C00-\x11C08\x11C0A-\x11C2E\x11C40\x11C72-\x11C8F\x11D00-\x11D06\x11D08-\x11D09\x11D0B-\x11D30\x11D46\x11D60-\x11D65\x11D67-\x11D68\x11D6A-\x11D89\x11D98\x11EE0-\x11EF2\x11FB0\x11FDD-\x11FE0\x12000-\x12399\x12400-\x1246E\x12480-\x12543\x13000-\x1342E\x14400-\x14646\x16800-\x16A38\x16A40-\x16A5E\x16AD0-\x16AED\x16B00-\x16B2F\x16B40-\x16B43\x16B63-\x16B77\x16B7D-\x16B8F\x16E40-\x16E7F\x16F00-\x16F4A\x16F50\x16F93-\x16F9F\x16FE0-\x16FE1\x16FE3\x17000-\x187F7\x18800-\x18CD5\x18D00-\x18D08\x1B000-\x1B11E\x1B150-\x1B152\x1B164-\x1B167\x1B170-\x1B2FB\x1BC00-\x1BC6A\x1BC70-\x1BC7C\x1BC80-\x1BC88\x1BC90-\x1BC99\x1D400-\x1D454\x1D456-\x1D49C\x1D49E-\x1D49F\x1D4A2\x1D4A5-\x1D4A6\x1D4A9-\x1D4AC\x1D4AE-\x1D4B9\x1D4BB\x1D4BD-\x1D4C3\x1D4C5-\x1D505\x1D507-\x1D50A\x1D50D-\x1D514\x1D516-\x1D51C\x1D51E-\x1D539\x1D53B-\x1D53E\x1D540-\x1D544\x1D546\x1D54A-\x1D550\x1D552-\x1D6A5\x1D6A8-\x1D6C0\x1D6C2-\x1D6DA\x1D6DC-\x1D6FA\x1D6FC-\x1D714\x1D716-\x1D734\x1D736-\x1D74E\x1D750-\x1D76E\x1D770-\x1D788\x1D78A-\x1D7A8\x1D7AA-\x1D7C2\x1D7C4-\x1D7CB\x1E100-\x1E12C\x1E137-\x1E13D\x1E14E\x1E2C0-\x1E2EB\x1E2FF\x1E800-\x1E8C4\x1E900-\x1E943\x1E94B\x1ECB0\x1EE00-\x1EE03\x1EE05-\x1EE1F\x1EE21-\x1EE22\x1EE24\x1EE27\x1EE29-\x1EE32\x1EE34-\x1EE37\x1EE39\x1EE3B\x1EE42\x1EE47\x1EE49\x1EE4B\x1EE4D-\x1EE4F\x1EE51-\x1EE52\x1EE54\x1EE57\x1EE59\x1EE5B\x1EE5D\x1EE5F\x1EE61-\x1EE62\x1EE64\x1EE67-\x1EE6A\x1EE6C-\x1EE72\x1EE74-\x1EE77\x1EE79-\x1EE7C\x1EE7E\x1EE80-\x1EE89\x1EE8B-\x1EE9B\x1EEA1-\x1EEA3\x1EEA5-\x1EEA9\x1EEAB-\x1EEBB\x20000-\x2A6DD\x2A700-\x2B734\x2B740-\x2B81D\x2B820-\x2CEA1\x2CEB0-\x2EBE0\x2F800-\x2FA1D\x30000-\x3134A]
$JavaIdentifierPart = [$JavaIdentifierStart\x00-\x08\x0E-\x1B\x30-\x39\x7F-\x9F\xAD\x300-\x36F\x483-\x487\x591-\x5BD\x5BF\x5C1-\x5C2\x5C4-\x5C5\x5C7\x600-\x605\x610-\x61A\x61C\x64B-\x669\x670\x6D6-\x6DD\x6DF-\x6E4\x6E7-\x6E8\x6EA-\x6ED\x6F0-\x6F9\x70F\x711\x730-\x74A\x7A6-\x7B0\x7C0-\x7C9\x7EB-\x7F3\x7FD\x816-\x819\x81B-\x823\x825-\x827\x829-\x82D\x859-\x85B\x8D3-\x903\x93A-\x93C\x93E-\x94F\x951-\x957\x962-\x963\x966-\x96F\x981-\x983\x9BC\x9BE-\x9C4\x9C7-\x9C8\x9CB-\x9CD\x9D7\x9E2-\x9E3\x9E6-\x9EF\x9FE\xA01-\xA03\xA3C\xA3E-\xA42\xA47-\xA48\xA4B-\xA4D\xA51\xA66-\xA71\xA75\xA81-\xA83\xABC\xABE-\xAC5\xAC7-\xAC9\xACB-\xACD\xAE2-\xAE3\xAE6-\xAEF\xAFA-\xAFF\xB01-\xB03\xB3C\xB3E-\xB44\xB47-\xB48\xB4B-\xB4D\xB55-\xB57\xB62-\xB63\xB66-\xB6F\xB82\xBBE-\xBC2\xBC6-\xBC8\xBCA-\xBCD\xBD7\xBE6-\xBEF\xC00-\xC04\xC3E-\xC44\xC46-\xC48\xC4A-\xC4D\xC55-\xC56\xC62-\xC63\xC66-\xC6F\xC81-\xC83\xCBC\xCBE-\xCC4\xCC6-\xCC8\xCCA-\xCCD\xCD5-\xCD6\xCE2-\xCE3\xCE6-\xCEF\xD00-\xD03\xD3B-\xD3C\xD3E-\xD44\xD46-\xD48\xD4A-\xD4D\xD57\xD62-\xD63\xD66-\xD6F\xD81-\xD83\xDCA\xDCF-\xDD4\xDD6\xDD8-\xDDF\xDE6-\xDEF\xDF2-\xDF3\xE31\xE34-\xE3A\xE47-\xE4E\xE50-\xE59\xEB1\xEB4-\xEBC\xEC8-\xECD\xED0-\xED9\xF18-\xF19\xF20-\xF29\xF35\xF37\xF39\xF3E-\xF3F\xF71-\xF84\xF86-\xF87\xF8D-\xF97\xF99-\xFBC\xFC6\x102B-\x103E\x1040-\x1049\x1056-\x1059\x105E-\x1060\x1062-\x1064\x1067-\x106D\x1071-\x1074\x1082-\x108D\x108F-\x109D\x135D-\x135F\x1712-\x1714\x1732-\x1734\x1752-\x1753\x1772-\x1773\x17B4-\x17D3\x17DD\x17E0-\x17E9\x180B-\x180E\x1810-\x1819\x1885-\x1886\x18A9\x1920-\x192B\x1930-\x193B\x1946-\x194F\x19D0-\x19D9\x1A17-\x1A1B\x1A55-\x1A5E\x1A60-\x1A7C\x1A7F-\x1A89\x1A90-\x1A99\x1AB0-\x1ABD\x1ABF-\x1AC0\x1B00-\x1B04\x1B34-\x1B44\x1B50-\x1B59\x1B6B-\x1B73\x1B80-\x1B82\x1BA1-\x1BAD\x1BB0-\x1BB9\x1BE6-\x1BF3\x1C24-\x1C37\x1C40-\x1C49\x1C50-\x1C59\x1CD0-\x1CD2\x1CD4-\x1CE8\x1CED\x1CF4\x1CF7-\x1CF9\x1DC0-\x1DF9\x1DFB-\x1DFF\x200B-\x200F\x202A-\x202E\x2060-\x2064\x2066-\x206F\x20D0-\x20DC\x20E1\x20E5-\x20F0\x2CEF-\x2CF1\x2D7F\x2DE0-\x2DFF\x302A-\x302F\x3099-\x309A\xA620-\xA629\xA66F\xA674-\xA67D\xA69E-\xA69F\xA6F0-\xA6F1\xA802\xA806\xA80B\xA823-\xA827\xA82C\xA880-\xA881\xA8B4-\xA8C5\xA8D0-\xA8D9\xA8E0-\xA8F1\xA8FF-\xA909\xA926-\xA92D\xA947-\xA953\xA980-\xA983\xA9B3-\xA9C0\xA9D0-\xA9D9\xA9E5\xA9F0-\xA9F9\xAA29-\xAA36\xAA43\xAA4C-\xAA4D\xAA50-\xAA59\xAA7B-\xAA7D\xAAB0\xAAB2-\xAAB4\xAAB7-\xAAB8\xAABE-\xAABF\xAAC1\xAAEB-\xAAEF\xAAF5-\xAAF6\xABE3-\xABEA\xABEC-\xABED\xABF0-\xABF9\xFB1E\xFE00-\xFE0F\xFE20-\xFE2F\xFEFF\xFF10-\xFF19\xFFF9-\xFFFB\x101FD\x102E0\x10376-\x1037A\x104A0-\x104A9\x10A01-\x10A03\x10A05-\x10A06\x10A0C-\x10A0F\x10A38-\x10A3A\x10A3F\x10AE5-\x10AE6\x10D24-\x10D27\x10D30-\x10D39\x10EAB-\x10EAC\x10F46-\x10F50\x11000-\x11002\x11038-\x11046\x11066-\x1106F\x1107F-\x11082\x110B0-\x110BA\x110BD\x110CD\x110F0-\x110F9\x11100-\x11102\x11127-\x11134\x11136-\x1113F\x11145-\x11146\x11173\x11180-\x11182\x111B3-\x111C0\x111C9-\x111CC\x111CE-\x111D9\x1122C-\x11237\x1123E\x112DF-\x112EA\x112F0-\x112F9\x11300-\x11303\x1133B-\x1133C\x1133E-\x11344\x11347-\x11348\x1134B-\x1134D\x11357\x11362-\x11363\x11366-\x1136C\x11370-\x11374\x11435-\x11446\x11450-\x11459\x1145E\x114B0-\x114C3\x114D0-\x114D9\x115AF-\x115B5\x115B8-\x115C0\x115DC-\x115DD\x11630-\x11640\x11650-\x11659\x116AB-\x116B7\x116C0-\x116C9\x1171D-\x1172B\x11730-\x11739\x1182C-\x1183A\x118E0-\x118E9\x11930-\x11935\x11937-\x11938\x1193B-\x1193E\x11940\x11942-\x11943\x11950-\x11959\x119D1-\x119D7\x119DA-\x119E0\x119E4\x11A01-\x11A0A\x11A33-\x11A39\x11A3B-\x11A3E\x11A47\x11A51-\x11A5B\x11A8A-\x11A99\x11C2F-\x11C36\x11C38-\x11C3F\x11C50-\x11C59\x11C92-\x11CA7\x11CA9-\x11CB6\x11D31-\x11D36\x11D3A\x11D3C-\x11D3D\x11D3F-\x11D45\x11D47\x11D50-\x11D59\x11D8A-\x11D8E\x11D90-\x11D91\x11D93-\x11D97\x11DA0-\x11DA9\x11EF3-\x11EF6\x13430-\x13438\x16A60-\x16A69\x16AF0-\x16AF4\x16B30-\x16B36\x16B50-\x16B59\x16F4F\x16F51-\x16F87\x16F8F-\x16F92\x16FE4\x16FF0-\x16FF1\x1BC9D-\x1BC9E\x1BCA0-\x1BCA3\x1D165-\x1D169\x1D16D-\x1D182\x1D185-\x1D18B\x1D1AA-\x1D1AD\x1D242-\x1D244\x1D7CE-\x1D7FF\x1DA00-\x1DA36\x1DA3B-\x1DA6C\x1DA75\x1DA84\x1DA9B-\x1DA9F\x1DAA1-\x1DAAF\x1E000-\x1E006\x1E008-\x1E018\x1E01B-\x1E021\x1E023-\x1E024\x1E026-\x1E02A\x1E130-\x1E136\x1E140-\x1E149\x1E2EC-\x1E2F9\x1E8D0-\x1E8D6\x1E944-\x1E94A\x1E950-\x1E959\x1FBF0-\x1FBF9\xE0001\xE0020-\xE007F\xE0100-\xE01EF]

-- Characters that could be part of a number literal
-- Note that this is intentionally boarder than the actual language.
-- This allows us to handle actions with more informative error messages.
$NumLitPart = [0-9A-Za-z_]

@Digits = $digit|$digit($digit|_)*$digit
@IntegerTypeSuffix = [lL]
@HexDigits = $hexdigit|$hexdigit($hexdigit|_)*$hexdigit
@HexNumeral = 0[xX]@HexDigits
@SignedInteger = [\+\-]?@Digits
@ExponentPart = [eE][\+\-]?@SignedInteger
@FloatTypeSuffix = [fFdD]
@HexSignificand = @HexNumeral\.?|0[xX]@HexDigits?\.@HexDigits
@BinaryExponent = [pP]@SignedInteger

-- loose match, relies on Alex action.
@EscapeBody = \\(.|[0-9]+)

tokens :-

  $JavaWhiteSpace+
    ;

  -- EndOfLineComment
  "//".*
    ;
  -- TraditionalComment
  -- See: [Regular expressions] section in /docs/README.md
  \/\*($NotStarNotSlash|\/|\*\**$NotStarNotSlash)*\*\**\/
    ;
  "true"
    { mkTokConst (BooleanLiteral True) }
  "false"
    { mkTokConst (BooleanLiteral False) }
  "null"
    { mkTokConst NullLiteral }

  -- Keywords
  "abstract"
    { mkTokConst KwAbstract }
  "continue"
    { mkTokConst KwContinue }
  "for"
    { mkTokConst KwFor }
  "new"
    { mkTokConst KwNew }
  "switch"
    { mkTokConst KwSwitch }
  "assert"
    { mkTokConst KwAssert }
  "default"
    { mkTokConst KwDefault }
  "if"
    { mkTokConst KwIf }
  "package"
    { mkTokConst KwPackage }
  "synchronized"
    { mkTokConst KwSynchronized }
  "boolean"
    { mkTokConst KwBoolean }
  "do"
    { mkTokConst KwDo }
  "goto"
    { mkTokConst KwGoto }
  "private"
    { mkTokConst KwPrivate }
  "this"
    { mkTokConst KwThis }
  "break"
    { mkTokConst KwBreak }
  "double"
    { mkTokConst KwDouble }
  "implements"
    { mkTokConst KwImplements }
  "protected"
    { mkTokConst KwProtected }
  "throw"
    { mkTokConst KwThrow }
  "byte"
    { mkTokConst KwByte }
  "else"
    { mkTokConst KwElse }
  "import"
    { mkTokConst KwImport }
  "public"
    { mkTokConst KwPublic }
  "throws"
    { mkTokConst KwThrows }
  "case"
    { mkTokConst KwCase }
  "enum"
    { mkTokConst KwEnum }
  "instanceof"
    { mkTokConst KwInstanceof }
  "return"
    { mkTokConst KwReturn }
  "transient"
    { mkTokConst KwTransient }
  "catch"
    { mkTokConst KwCatch }
  "extends"
    { mkTokConst KwExtends }
  "int"
    { mkTokConst KwInt }
  "short"
    { mkTokConst KwShort }
  "try"
    { mkTokConst KwTry }
  "char"
    { mkTokConst KwChar }
  "final"
    { mkTokConst KwFinal }
  "interface"
    { mkTokConst KwInterface }
  "static"
    { mkTokConst KwStatic }
  "void"
    { mkTokConst KwVoid }
  "class"
    { mkTokConst KwClass }
  "finally"
    { mkTokConst KwFinally }
  "long"
    { mkTokConst KwLong }
  "strictfp"
    { mkTokConst KwStrictfp }
  "volatile"
    { mkTokConst KwVolatile }
  "const"
    { mkTokConst KwConst }
  "float"
    { mkTokConst KwFloat }
  "native"
    { mkTokConst KwNative }
  "super"
    { mkTokConst KwSuper }
  "while"
    { mkTokConst KwWhile }
  "_"
    { mkTokConst KwSymbolUnderscore }

  -- Separators
  "("
    { mkTokConst SepLParen }
  ")"
    { mkTokConst SepRParen }
  "{"
    { mkTokConst SepLBrace }
  "}"
    { mkTokConst SepRBrace }
  "["
    { mkTokConst SepLBracket }
  "]"
    { mkTokConst SepRBracket }
  ";"
    { mkTokConst SepSColon }
  ","
    { mkTokConst SepComma }
  "."
    { mkTokConst SepDot }
  "..."
    { mkTokConst SepTripleDot }
  "@"
    { mkTokConst SepAt }
  "::"
    { mkTokConst SepDbColon }

  -- Operators
  "="
    { mkTokConst OpEq }
  ">"
    { mkTokConst OpGt }
  "<"
    { mkTokConst OpLt }
  "!"
    { mkTokConst OpExclam }
  "~"
    { mkTokConst OpTilde }
  "?"
    { mkTokConst OpQue }
  ":"
    { mkTokConst OpCol }
  "->"
    { mkTokConst OpMinusGt }
  "=="
    { mkTokConst OpEqEq }
  "<="
    { mkTokConst OpLe }
  "!="
    { mkTokConst OpNe }
  "&&"
    { mkTokConst OpAndAnd }
  "||"
    { mkTokConst OpOrOr }
  "++"
    { mkTokConst OpPlusPlus }
  "--"
    { mkTokConst OpMinusMinus }
  "+"
    { mkTokConst OpPlus }
  "-"
    { mkTokConst OpMinus }
  "*"
    { mkTokConst OpStar }
  "/"
    { mkTokConst OpSlash }
  "&"
    { mkTokConst OpAnd }
  "|"
    { mkTokConst OpOr }
  "^"
    { mkTokConst OpCaret }
  "%"
    { mkTokConst OpPercent }
  "<<"
    { mkTokConst OpLtLt }
  "+="
    { mkTokConst OpPlusEq }
  "-="
    { mkTokConst OpMinusEq }
  "*="
    { mkTokConst OpStarEq }
  "/="
    { mkTokConst OpSlashEq }
  "&="
    { mkTokConst OpAndEq }
  "|="
    { mkTokConst OpOrEq }
  "^="
    { mkTokConst OpCaretEq }
  "%="
    { mkTokConst OpPercentEq }
  "<<="
    { mkTokConst OpLtLtEq }

  -- CharacterLiteral
  --   SingleCharacter
  '[^'\\]'
    { mkTok getCharLiteral }
  --   EscapeSequence
  "'"@EscapeBody"'"
    { mkTok getCharLiteral }

  -- StringLiteral
  \"([^\\\"]|@EscapeBody)*\"
    { mkTok getStringLiteral }

  -- TextBlock
  -- See: [Regular expressions] section in /docs/README.md
  \"\"\"($NotDbQuoteNotSlash|\\(\"|\\|$NotDbQuoteNotSlash)|\"($NotDbQuoteNotSlash|\\(\"|\\|$NotDbQuoteNotSlash)|\"($NotDbQuoteNotSlash|\\(\"|\\|$NotDbQuoteNotSlash))))*\"\"\"
    { mkTok getTextBlock }

  -- IntegerLiteral
  --   DecimalIntegerLiteral
  --     note that the regex below is not yet ready to be merged with others
  --     as it would interfere with floating point literals.
  [0-9][_0-9]*@IntegerTypeSuffix?
    { mkTok getIntegerLiteral }
  --   HexIntegerLiteral
  --   OctalIntegerLiteral
  --   BinaryIntegerLiteral
  0[bBxX]?$NumLitPart+@IntegerTypeSuffix?
    { mkTok getIntegerLiteral }

  -- FloatingPointLiteral
  --   DecimalFloatingPointLiteral
  @Digits\.@Digits?@ExponentPart?@FloatTypeSuffix?
    { mkTok getFloatingPoint }
  --   DecimalFloatingPointLiteral
  \.@Digits@ExponentPart?@FloatTypeSuffix?
    { mkTok getFloatingPoint }
  --   DecimalFloatingPointLiteral
  @Digits@ExponentPart@FloatTypeSuffix?
    { mkTok getFloatingPoint }
  --   DecimalFloatingPointLiteral
  @Digits@ExponentPart?@FloatTypeSuffix
    { mkTok getFloatingPoint }
  --  HexadecimalFloatingPointLiteral
  @HexSignificand@BinaryExponent@FloatTypeSuffix?
    { mkTok getFloatingPoint }

  $JavaIdentifierStart$JavaIdentifierPart*
    { mkTok (pure . Identifier) }
{

alexEOF :: Alex Token
alexEOF = pure EndOfFile

instance MonadError String Alex where
  throwError e = Alex (const (Left e))
  catchError m handler = Alex $ \st -> case unAlex m st of
    Left e -> unAlex (handler e) st
    Right v -> Right v

instance MonadState AlexState Alex where
  state f = Alex $ \s -> let (v, s') = f s in pure (s', v)

alex_actions :: Array Int (AlexAction Token)

}
