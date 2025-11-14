
/*!
 * Color mode toggler for Bootstrap's docs (https://getbootstrap.com/)
 * Copyright 2011-2023 The Bootstrap Authors
 * Licensed under the Creative Commons Attribution 3.0 Unported License.
 * Updates for {pkgdown} by the {bslib} authors, also licensed under CC-BY-3.0.
 */

const getStoredTheme = () => localStorage.getItem('theme')
const setStoredTheme = theme => localStorage.setItem('theme', theme)

const getPreferredTheme = () => {
  const storedTheme = getStoredTheme()
  if (storedTheme) {
    return storedTheme
  }

  return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'
}

const setTheme = theme => {
  if (theme === 'auto') {
    document.documentElement.setAttribute('data-bs-theme', (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'))
  } else {
    document.documentElement.setAttribute('data-bs-theme', theme)
  }
}

function bsSetupThemeToggle() {
  'use strict'

  const showActiveTheme = (theme, focus = false) => {
    var activeLabel, activeIcon;

    document.querySelectorAll('[data-bs-theme-value]').forEach(element => {
      const buttonTheme = element.getAttribute('data-bs-theme-value')
      const isActive = buttonTheme == theme

      element.classList.toggle('active', isActive)
      element.setAttribute('aria-pressed', isActive)

      if (isActive) {
        activeLabel = element.textContent;
        activeIcon = element.querySelector('span').classList.value;
      }
    })

    const themeSwitcher = document.querySelector('#dropdown-lightswitch')
    if (!themeSwitcher) {
      return
    }

    themeSwitcher.setAttribute('aria-label', activeLabel)
    themeSwitcher.querySelector('span').classList.value = activeIcon;

    if (focus) {
      themeSwitcher.focus()
    }
  }

  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => {
    const storedTheme = getStoredTheme()
    if (storedTheme !== 'light' && storedTheme !== 'dark') {
      setTheme(getPreferredTheme())
    }
  })

  window.addEventListener('DOMContentLoaded', () => {
    showActiveTheme(getPreferredTheme())

    document
      .querySelectorAll('[data-bs-theme-value]')
      .forEach(toggle => {
        toggle.addEventListener('click', () => {
          const theme = toggle.getAttribute('data-bs-theme-value')
          setTheme(theme)
          setStoredTheme(theme)
          showActiveTheme(theme, true)
        })
      })
  })
}

setTheme(getPreferredTheme());
bsSetupThemeToggle();
