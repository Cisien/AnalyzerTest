using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace JsonExampleAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class JsonExampleAnalyzerAnalyzer : DiagnosticAnalyzer
    {
        private const string Category = "Documentation";

        private static readonly DiagnosticDescriptor _exampleMissingRule = new DiagnosticDescriptor("WA0001", "Example Missing", "<example> missing for class {0}", Category, DiagnosticSeverity.Error, true);
        private static readonly DiagnosticDescriptor _codeMissingRule = new DiagnosticDescriptor("WA0002", "Code Missing", "<code> missing for class {0}", Category, DiagnosticSeverity.Error, true);
        private static readonly DiagnosticDescriptor _codeMissingClassRule = new DiagnosticDescriptor("WA0003", "Code Class Missing", "<code> class attribute missing for class {0}", Category, DiagnosticSeverity.Error, true);
        private static readonly DiagnosticDescriptor _incorrectClassRule = new DiagnosticDescriptor("WA0004", "Code Class Incorrect", "Incorrect <code> class attribute value. Expected 'javascript', but received '{1}' for class {0}", Category, DiagnosticSeverity.Error, true);
        private static readonly DiagnosticDescriptor _jsonExampleParsingFailureRule = new DiagnosticDescriptor("WA0005", "Invalid Example Json", "Unable to parse Json in example code: {1} for class {0}", Category, DiagnosticSeverity.Error, true);
        private static readonly DiagnosticDescriptor _jsonPropertyMissingRule = new DiagnosticDescriptor("WA0006", "Property Missing", "'{1}' does not have a corresponding json exmaple for class {0}", Category, DiagnosticSeverity.Error, true);
        private static readonly DiagnosticDescriptor _extraJsonPropertyRule = new DiagnosticDescriptor("WA0007", "Property Extra", "'{1}' is not defined in class {0}", Category, DiagnosticSeverity.Error, true);

        private readonly ImmutableArray<DiagnosticDescriptor> _rules = ImmutableArray.Create(
            _exampleMissingRule,
            _codeMissingRule,
            _codeMissingClassRule,
            _incorrectClassRule,
            _jsonExampleParsingFailureRule,
            _jsonPropertyMissingRule,
            _extraJsonPropertyRule
        );

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => _rules;

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.ReportDiagnostics);

            context.RegisterSyntaxNodeAction(AnalyzeSymbol, SyntaxKind.ClassDeclaration);
        }

        private static void AnalyzeSymbol(SyntaxNodeAnalysisContext context)
        {
            var node = context.Node as ClassDeclarationSyntax;
            var location = node.Identifier.GetLocation();
            if (!node.Modifiers.Any(a => a.IsKind(SyntaxKind.PublicKeyword)))
            {
                return;
            }

            if (!node.HasStructuredTrivia)
            {
                return;
            }

            var docTrivia = node.GetLeadingTrivia()
                .Select(a => a.GetStructure())
                .OfType<DocumentationCommentTriviaSyntax>()
                .FirstOrDefault();

            var xmlNodes = docTrivia.ChildNodes().OfType<XmlElementSyntax>();


            var example = xmlNodes.FirstOrDefault(a => a.StartTag.Name.LocalName.ValueText == "example");
            if (example == null)
            {
                context.ReportDiagnostic(Diagnostic.Create(_exampleMissingRule, location, node.Identifier.ValueText));
                return;
            }

            var exampleChildren = example.ChildNodes().OfType<XmlElementSyntax>();
            var code = exampleChildren.FirstOrDefault(a => a.StartTag.Name.LocalName.ValueText == "code");
            if (code == null)
            {
                context.ReportDiagnostic(Diagnostic.Create(_codeMissingRule, location, node.Identifier.ValueText));
                return;
            }

            if (!(code.StartTag.Attributes.FirstOrDefault(a => a.Name.LocalName.ValueText == "class") is XmlTextAttributeSyntax lang))
            {
                context.ReportDiagnostic(Diagnostic.Create(_codeMissingClassRule, location, node.Identifier.ValueText));
                return;
            }

            var langType = lang.TextTokens.FirstOrDefault().ValueText;

            if (langType != "javascript")
            {
                context.ReportDiagnostic(Diagnostic.Create(_incorrectClassRule, location, node.Identifier.ValueText, langType));
                return;
            }

            var content = code.Content.FirstOrDefault() as XmlTextSyntax;
            var text = string.Join(string.Empty, content.TextTokens);

            JObject json;
            try
            {
                json = JObject.Parse(text);
            }
            catch (JsonException ex)
            {
                context.ReportDiagnostic(Diagnostic.Create(_jsonExampleParsingFailureRule, location, node.Identifier.ValueText, ex.Message));
                return;
            }

            var childNodes = node.ChildNodes().OfType<PropertyDeclarationSyntax>()
                .Where(prop => prop.Modifiers.Any(modifier => modifier.IsKind(SyntaxKind.PublicKeyword))).ToList();

            foreach (var member in childNodes)
            {
                if (member.AttributeLists.SelectMany(a => a.Attributes).Select(a => a.Name).OfType<IdentifierNameSyntax>().Any(attrib => attrib.Identifier.Text == "Obsolete"))
                {
                    continue;
                }

                if (!json.TryGetValue(member.Identifier.ValueText, out _))
                {
                    context.ReportDiagnostic(Diagnostic.Create(_jsonPropertyMissingRule, member.Identifier.GetLocation(), node.Identifier.ValueText, member.Identifier.ValueText));
                }
            }

            foreach (var jsonProperty in json.Properties())
            {
                if (childNodes.Any(prop => prop.Identifier.ValueText == jsonProperty.Name && prop.AttributeLists.SelectMany(a => a.Attributes)
                    .Select(a => a.Name).OfType<IdentifierNameSyntax>().Any(attrib => attrib.Identifier.Text == "Obsolete")))
                {
                    continue;
                }

                if (childNodes.All(a => a.Identifier.ValueText != jsonProperty.Name))
                {
                    context.ReportDiagnostic(Diagnostic.Create(_extraJsonPropertyRule, location, node.Identifier.ValueText, jsonProperty.Name));
                }

                var member = childNodes.SingleOrDefault(a => a.Identifier.Text == jsonProperty.Name);
                if(member == null)
                {
                    continue;
                }

                var thing = 1 + 1;
            }

        }
    }
}
