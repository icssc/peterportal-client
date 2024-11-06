import { useCookies } from 'react-cookie';

export function useIsLoggedIn() {
  const [cookies] = useCookies(['user']);

  return cookies.user !== undefined;
}
