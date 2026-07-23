import type { CSSProperties } from 'react';

export enum TourStepName {
  welcome = 'welcome',
  year = 'year',
  firstYearTransfer = 'firstYearTransfer',
  nonFirstYearImport = 'nonFirstYearImport',
}

export type TutorialBranch = 'firstYear' | 'nonFirstYear';
export type TutorialButtonAction = 'none' | 'close' | 'next' | 'back';
export type TutorialVariant = 'welcome' | 'step';

export const TOUR_STEP = {
  // add new steps here
  welcome: 0,
  year: 1,
  firstYearTransfer: 2,
  nonFirstYearImport: 3,
} as const;

const ROADMAP_TOUR_HAS_RUN_KEY = 'roadmap__tutorial_has_run';
const TUTORIAL_OUTLINE = 'tutorial-highlight-outline';

// Storage helpers
function setLocalStorageTourHasRun(value: 'true' | 'false') {
  if (typeof window === 'undefined') return;
  localStorage.setItem(ROADMAP_TOUR_HAS_RUN_KEY, value);
}

export function markTourHasRun() {
  setLocalStorageTourHasRun('true');
}

// COMMENTED FOR TESTING PURPOSES

// function getLocalStorageTourHasRun() {
//   if (typeof window === 'undefined') return null;
//   return localStorage.getItem(ROADMAP_TOUR_HAS_RUN_KEY);
// }

export function tourShouldRun(): boolean {
  if (typeof window === 'undefined') return false;
  return !(
    // commented out for testing purposes

    // getLocalStorageTourHasRun() === 'true' ||
    window.matchMedia('(max-width: 799px)').matches
  );
}

// DOM outline helpers
export function addTutorialOutline(selector: string) {
  document.querySelector(selector)?.classList.add(TUTORIAL_OUTLINE);
}

export function removeTutorialOutline(selector: string) {
  document.querySelector(selector)?.classList.remove(TUTORIAL_OUTLINE);
}

// Styles
export const variantPopoverStyle: Record<TutorialVariant, CSSProperties> = {
  welcome: {
    width: 366,
    height: 240,
    borderRadius: 13,
    border: '2px solid rgba(184, 184, 184, 0.5)',
    padding: '28px 24px 42px 24px',
    gap: 10,
    boxSizing: 'border-box',
    overflow: 'visible',
  },
  step: {
    borderRadius: 10,
    border: '1px solid rgba(184, 184, 184, 0.5)',
    padding: '28px 40px 30px 40px',
    boxSizing: 'border-box',
    overflow: 'visible',
  },
};
