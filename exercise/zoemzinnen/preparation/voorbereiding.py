from random import randint, choice, sample
from solution import zoemzin1, zoemzin2

generators = (
    (
        'buzz', 
        (
            ('integrated', 'total', 'systematized', 'parallel', 'functional', 'responsive', 'optional', 'synchronized', 'compatible', 'balanced'),
            ('management', 'organizational', 'monitored', 'reciprocal', 'digital', 'logistical', 'transitional', 'incremental', 'policy'),
            ('options', 'flexibility', 'capability', 'mobility', 'programming', 'concept', 'time-phase', 'projection', 'hardware', 'contingency')
        )
    ),
    (
        'corporate-speak', 
        (
            ('appropriately', 'assertively', 'authoritatively', 'collaboratively', 'compellingly', 'competently', 'completely', 'continually', 'conveniently', 'credibly', 'distinctively', 'dramatically', 'dynamically', 'efficiently', 'energistically', 'enthusiastically', 'fungibly', 'globally', 'holistically', 'interactively', 'intrinsically', 'monotonectally', 'objectively', 'phosfluorescently', 'proactively', 'professionally', 'progressively', 'quickly', 'rapidiously', 'seamlessly', 'synergistically', 'uniquely'),
            ('actualize', 'administrate', 'aggregate', 'architect', 'benchmark', 'brand', 'build', 'cloudify', 'communicate', 'conceptualize', 'coordinate', 'create', 'cultivate', 'customize', 'deliver', 'deploy', 'develop', 'dinintermediate', 'disseminate', 'drive', 'embrace', 'e-enable', 'empower', 'enable', 'engage', 'engineer', 'enhance', 'envisioneer', 'evisculate', 'evolve', 'expedite', 'exploit', 'extend', 'fabricate', 'facilitate', 'fashion', 'formulate', 'foster', 'generate', 'grow', 'harness', 'impact', 'implement', 'incentivize', 'incubate', 'initiate', 'innovate', 'integrate', 'iterate', 'maintain', 'matrix', 'maximize', 'mesh', 'monetize', 'morph', 'myocardinate', 'negotiate', 'network', 'optimize', 'orchestrate', 'plagiarize', 'pontificate', 'predominate', 'procrastinate', 'productivate', 'productize', 'promote', 'pursue', 'recaptiualize', 'reconceptualize', 'redefine', 're-engineer', 'reintermediate', 'reinvent', 'repurpose', 'restore', 'revolutionize', 'scale', 'seize', 'simplify', 'strategize', 'streamline', 'supply', 'syndicate', 'synergize', 'synthesize', 'target', 'transform', 'transition', 'underwhelm', 'unleash', 'utilize', 'visualize', 'whiteboard'),
            ('24/7', '24/365', 'accurate', 'adaptive', 'alternative', 'B2B', 'B2C', 'backend', 'backward-compatible', 'best-of-breed', 'bleeding-edge', 'bricks-and-clicks', 'business', 'clicks-and-mortar', 'client-based', 'client-centered', 'client-centric', 'client-focused', 'collaborative', 'compelling', 'competitive', 'cooperative', 'corporate', 'cost-effective', 'covalent', 'cross-functional', 'cross-media', 'cross-platform', 'cross-unit', 'customer-directed', 'customized', 'cutting-edge', 'distinctive', 'distributed', 'diverse', 'dynamic', 'e-business', 'effective', 'efficient', 'elastic', 'emerging', 'empowered', 'enabled', 'end-to-end', 'enterprise', 'enterprise-wide', 'error-free', 'ethical', 'excellent', 'exceptional', 'extensible', 'extensive', 'flexible', 'focused', 'frictionless', 'front-end', 'functional', 'functionalized', 'fungible', 'future-proof', 'global', 'goal-oriented', 'granular', 'high-payoff', 'hyperscale', 'high-quality', 'holistic', 'impactful', 'inexpensive', 'innovative', 'integrated', 'interactive', 'interdependent', 'intermandated', 'interoperable', 'intuitive', 'just-in-time', 'leading-edge', 'leveraged', 'long-term', 'high-impact', 'low-risk', 'high-yield', 'magnetic', 'maintainable', 'market-driven', 'mission-critical', 'multidisciplinary', 'multifunctional', 'multimedia-based', 'next-generation', 'on-demand', 'one-to-one', 'open-source', 'optimal', 'orthogonal', 'out-of-the-box', 'pandemic', 'parallel', 'plug-and-play', 'premier', 'premium', 'principle-centered', 'proactive', 'process-centric', 'professional', 'progressive', 'prospective', 'quality', 'real-time', 'reliable', 'resource-leveling', 'revolutionary', 'robust', 'scalable', 'seamless', 'stand-alone', 'standardized', 'standards-compliant', 'state-of-the-art', 'sticky', 'strategic', 'superior', 'sustainable', 'synergistic', 'tactical', 'team-building', 'team-driven', 'timely', 'top-line', 'transparent', 'turnkey', 'ubiquitous', 'unique', 'user-centric', 'user-friendly', 'value-added', 'vertical', 'viral', 'virtual', 'visionary', 'web-enabled', 'wireless', 'world-class', 'worldwide'),
            ('alignments', 'applications', 'architectures', 'bandwidth', 'benefits', 'best-practices', 'channels', 'clouds', 'communities', 'content', 'convergence', 'data', 'deliverables', 'e-business', 'e-commerce', 'e-markets', 'e-tailers', 'e-services', 'experiences', 'expertise', 'functionalities', 'fungibility', 'ideas', 'imperatives', 'infomediaries', 'information', 'infrastructures', 'initiatives', 'innovation', 'interfaces', 'leadership', 'markets', 'materials', 'meta-services', 'methodologies', 'metrics', 'mindshare', 'models', 'networks', 'niches', 'nosql', 'opportunities', 'outsourcing', 'paradigms', 'partnerships', 'platforms', 'portals', 'potentialities', 'processes', 'products', 'relationships', 'resources', 'results', 'ROI', 'scenarios', 'schemas', 'services', 'solutions', 'sources', 'storage', 'synergy', 'systems', 'technologies', 'technology', 'users', 'value', 'vortals', 'web-readiness')
        )
    ),
    (
        'shakespeare', 
        (
            ('Thou',),
            ('artless', 'bawdy', 'beslubbering', 'bootless', 'churlish', 'cockered', 'clouted', 'craven', 'currish', 'dankish', 'dissembling', 'droning', 'errant', 'fawning', 'fobbing', 'froward', 'frothy', 'gleeking', 'goatish', 'gorbellied', 'impertinent', 'infectious', 'jarring', 'loggerheaded', 'lumpish', 'mammering', 'mangled', 'mewling', 'paunchy', 'pribbling', 'puking', 'puny', 'quailing', 'rank', 'reeky', 'roguish', 'ruttish', 'saucy', 'spleeny', 'spongy', 'surly', 'tottering', 'unmuzzled', 'vain', 'venomed', 'villainous', 'warped', 'wayward', 'weedy', 'yeasty'),
            ('base-court', 'bat-fowling', 'beef-witted', 'beetle-headed', 'boil-brained', 'clapper-clawed', 'clay-brained', 'common-kissing', 'crook-pated', 'dismal-dreaming', 'dizzy-eyed', 'doghearted', 'dread-bolted', 'earth-vexing', 'elf-skinned', 'fat-kidneyed', 'fen-sucked', 'flap-mouthed', 'fly-bitten', 'folly-fallen', 'fool-born', 'full-gorged', 'guts-griping', 'half-faced', 'hasty-witted', 'hedge-born', 'hell-hated', 'idle-headed', 'ill-breeding', 'ill-nurtured', 'knotty-pated', 'milk-livered', 'motley-minded', 'onion-eyed', 'plume-plucked', 'pottle-deep', 'pox-marked', 'reeling-ripe', 'rough-hewn', 'rude-growing', 'rump-fed', 'shard-borne', 'sheep-biting', 'spur-galled', 'swag-bellied', 'tardy-gaited', 'tickle-brained', 'toad-spotted', 'urchin-snouted', 'weather-bitten'),
            ('apple-john', 'baggage', 'barnacle', 'bladder', 'boar-pig', 'bugbear', 'bum-bailey', 'canker-blossom', 'clack-dish', 'clotpole', 'coxcomb', 'codpiece', 'death-token', 'dewberry', 'flap-dragon', 'flax-wench', 'flirt-gill', 'foot-licker', 'fustilarian', 'giglet', 'gudgeon', 'haggard', 'harpy', 'hedge-pig', 'horn-beast', 'hugger-mugger', 'jolthead', 'lewdster', 'lout', 'maggot-pie', 'malt-worm', 'mammet', 'measle', 'minnow', 'miscreant', 'moldwarp', 'mumble-news', 'nut-hook', 'pigeon-egg', 'pignut', 'puttock', 'pumpion', 'ratsbane', 'scut', 'skainsmate', 'strumpet', 'varlet', 'vassal', 'whey-face', 'wagtail')
        )
    )
)

# generate some random test cases
cases = []
while len(cases) < 50:
    
    generator, wordlist = choice(generators)
    wordlist = tuple(tuple(sample(words, min(len(words), randint(3, 5)))) for words in wordlist)
    cases.append(wordlist)


def list_to_values(word_list):
    result = []
    for word in word_list:
        result.append({
            "type": "text",
            "data": word
        })
    return result


plan = dict()
plan["tabs"] = []

# generate test cases for function buzzphrase1
contexts = []
buzzphrase1 = {
    "name": "Zoemzin 1",
    "contexts": contexts
}
plan["tabs"].append(buzzphrase1)
for wordlist in cases:

    wordlist = list(list(w) for w in wordlist)

    wordlist_arguments = []
    for word_listing in wordlist:
        wordlist_arguments.append({
            "type": "list",
            "data": list_to_values(word_listing)
        })

    argument = {
        "type": "list",
        "data": wordlist_arguments
    }

    function_ = {
        "type": "top",
        "name": "zoemzin1",
        "arguments": [argument]
    }
    input_ = {
        "function": function_
    }

    expected = {
        "type": "text",
        "data": zoemzin1(wordlist)
    }

    output_channel = {
        "value": expected,
        "evaluator": {
            "language": "python",
            "path": "./buzzchecker.py",
            "arguments": [argument],
            "type": "custom"
        }
    }

    output_ = {
        "result": output_channel
    }

    testcase = {
        "input": input_,
        "output": output_
    }

    context = {
        "normal": [testcase]
    }
    contexts.append(context)
    
# generate test cases for function buzzphrase2
contexts = []
buzzphrase1 = {
    "name": "Zoemzin 2",
    "contexts": contexts
}
plan["tabs"].append(buzzphrase1)
for wordlist in cases:

    wordlist = list(list(w) for w in wordlist)

    arguments = []
    for list_ in wordlist:
        arguments.append({
            "type": "list",
            "data": list_to_values(list_)
        })

    function_ = {
        "type": "top",
        "name": "zoemzin2",
        "arguments": arguments
    }
    input_ = {
        "function": function_
    }

    expected = {
        "type": "text",
        "data": zoemzin2(*wordlist)
    }

    eval_args = {
        "type": "list",
        "data": arguments
    }

    output_channel = {
        "value": expected,
        "evaluator": {
            "language": "python",
            "path": "./buzzchecker.py",
            "arguments": [eval_args],
            "type": "custom"
        }
    }

    output_ = {
        "result": output_channel
    }

    testcase = {
        "input": input_,
        "output": output_
    }

    context = {
        "normal": [testcase]
    }
    contexts.append(context)


# Print testplan to json.

import json
with open('plan.json', 'w') as fp:
    json.dump(plan, fp)
