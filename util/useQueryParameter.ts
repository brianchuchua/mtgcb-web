import { useRouter } from 'next/router';

export const useQueryParameter = (): ((key: string, value: string) => void) => {
  const router = useRouter();

  const setQueryParameter = (key: string, value: string) => {
    if (value === '') {
      const { [key]: _, ...query } = router.query;
      router.replace({ query }, undefined, { shallow: true });
    } else {
      router.replace({ query: { ...router.query, [key]: value } }, undefined, { shallow: true });
    }
  };

  return setQueryParameter;
};
