import { redirect } from 'next/navigation';

/*
    Refer to next.config.mjs for the main method of redirection
  */
export default function Page() {
  redirect('/');
}
