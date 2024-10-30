import Toastify from 'toastify-js';

export default function toaster() {
  return (text: string, style?: Record<string, string>, callback?: () => void) =>
    Toastify({
      text,
      duration: 3000,
      close: true,
      gravity: 'bottom',
      position: 'right',
      style: { background: 'var(--peterportal-primary-color-1)', fontWeight: 'bold', ...style },
      onClick: callback,
    }).showToast();
}
