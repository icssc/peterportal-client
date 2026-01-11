import { useAppSelector } from '../store/hooks';

export function useCurrentPreview() {
  const previews = useAppSelector((state) => state.coursePreview.previewStack);

  const currentPreview = previews.length ? previews[previews.length - 1] : null;
  return currentPreview;
}
