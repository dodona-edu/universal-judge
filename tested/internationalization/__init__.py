import i18n
import os.path

i18n.set('filename_format', '{locale}.{format}')
i18n.set('file_format', 'yaml')
i18n.set('locale', 'en')
i18n.set('fallback', 'en')
i18n.set('enable_memoization', True)
i18n.load_path.append(os.path.dirname(__file__))


def set_locale(locale: str):
    i18n.set('locale', locale)


def get_i18n_string(key: str, **kwargs) -> str:
    return i18n.t(key, **kwargs)
