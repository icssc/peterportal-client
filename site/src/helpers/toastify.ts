import Toastify from 'toastify-js';

export default function spawnToast(text: string, error = false, style?: Record<string, string>, callback?: () => void) {
  return Toastify({
    text,
    duration: 3000,
    close: true,
    gravity: 'bottom',
    position: 'right',
    style: {
      background: error ? 'var(--red-primary)' : 'var(--blue-primary)',
      fontWeight: 'bold',
      ...style,
    },
    onClick: callback,
  }).showToast();
}
