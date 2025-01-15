import { ReactSortableProps, SortableOptions } from 'react-sortablejs';
import { CourseGQLData } from '../types/types';

const baseSortable: SortableOptions = {
  animation: 150,
  forceFallback: true,
  fallbackOnBody: true,
  filter: '.btn',
};

export const quarterSortable: SortableOptions & Partial<ReactSortableProps<CourseGQLData>> = {
  ...baseSortable,
  setList: () => {},
  group: { name: 'courses' },
};

export const courseSearchSortable: SortableOptions & Partial<ReactSortableProps<CourseGQLData>> = {
  ...baseSortable,
  setList: () => {},
  sort: false,
  revertOnSpill: true,
  group: { name: 'courses', pull: 'clone', put: false },
};
