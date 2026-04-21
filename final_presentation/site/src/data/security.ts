export type SecurityFeature = {
  title: string;
  detail: string;
  citation: string;
  icon: string;
};

export const SECURITY_FEATURES: readonly SecurityFeature[] = [
  {
    title: 'Password Policy',
    detail:
      'Every password must be 8-12 characters and contain at least one uppercase letter, one digit, and one special character. Enforced character-by-character before any account is saved.',
    citation: 'src/AUTH.cpy:230-275',
    icon: 'shield',
  },
  {
    title: 'Account Cap',
    detail:
      'The create-account path short-circuits once WS-ACCOUNT-COUNT reaches WS-CONST-MAX-ACCOUNTS (5) and prints a polite "come back later" message instead of accepting a sixth signup.',
    citation: 'src/AUTH.cpy:129-141',
    icon: 'lock',
  },
  {
    title: 'Masked Password Echo',
    detail:
      'Raw input is captured into WS-PASSWORD-INPUT but the output log only ever sees "********" — passwords never land in OUTPUT.TXT, test logs, or the audit trail.',
    citation: 'src/AUTH.cpy:210-212',
    icon: 'eye-off',
  },
  {
    title: 'Recipient Isolation',
    detail:
      'The view-messages loop compares MSG-RECIPIENT against the currently logged-in user on every record — a foreign recipient can never read someone else\'s inbox.',
    citation: 'src/VIEWMESSAGE.cpy:75-120',
    icon: 'inbox',
  },
  {
    title: 'Connection-Gated Messaging',
    detail:
      'Before a message can be written, 7820-VALIDATE-RECIPIENT walks CONNECTIONS.DAT in both directions (A→B and B→A) to confirm the sender and recipient are actually connected.',
    citation: 'src/SENDMESSAGE.cpy:198-225',
    icon: 'link',
  },
  {
    title: 'Duplicate-Application Guard',
    detail:
      '5327-CHECK-DUPLICATE-APPLICATION scans APPLICATIONS.DAT for a record matching the current user and selected job ID before allowing a second apply to the same posting.',
    citation: 'src/APPLYJOB.cpy:73-109',
    icon: 'check',
  },
] as const;
