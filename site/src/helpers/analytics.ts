import type { PostHog } from 'posthog-js';

export const analyticsEnum = {
  customCards: {
    title: 'Custom Cards',
    actions: {
      CREATE_CARD: 'Create Custom Card',
    },
  },
  transcript: {
    title: 'Transcript',
    actions: {
      IMPORT: 'Import Transcript',
    },
  },
  roadmap: {
    title: 'Roadmap',
    actions: {
      SAVE_LOGGED_OUT: 'Save Roadmap Logged Out',
    },
  },
} as const;

type AnalyticsEnum = typeof analyticsEnum;
export type AnalyticsCategory = AnalyticsEnum[keyof AnalyticsEnum];

export interface AnalyticsProps {
  category: AnalyticsCategory;
  action: string;
}

export function logAnalytics(postHog: PostHog | undefined, { category, action }: AnalyticsProps) {
  if (!postHog) return;
  postHog.capture(action, { category: category.title });
}
